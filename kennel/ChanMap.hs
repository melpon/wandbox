module ChanMap (
  ChanMap,
  newChanMap,
  insertLookup,
  writeChan
) where

import Import
import qualified Data.IORef                             as IORef
import qualified Network.Wai.EventSource                as EventSource
import qualified Control.Concurrent                     as Concurrent
import qualified Control.Exception                      as Exc
import qualified Control.Concurrent.Chan                as Chan
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import qualified System.Mem                             as Mem

type ChanEvent = Chan.Chan EventSource.ServerEvent
data ChanMap = ChanMap (IORef.IORef (Map.Map T.Text (ChanEvent, [Concurrent.ThreadId])))

newChanMap :: IO ChanMap
newChanMap = do
  cm <- ChanMap <$> IORef.newIORef Map.empty
  return cm

setTimeout :: ChanMap -> T.Text -> IO ()
setTimeout cm k = do
    -- wait 2 minutes
    -- FIXME: This timeout should be elonged by timing of any data transmitted.
    Concurrent.threadDelay (2*60*1000*1000)
    `Exc.finally`
    writeChan cm k EventSource.CloseEvent

heartbeat :: ChanEvent -> IO ()
heartbeat chan = do
    -- send an empty 2 times to force flush
    Chan.writeChan chan $ EventSource.ServerEvent Nothing Nothing []
    Chan.writeChan chan $ EventSource.ServerEvent Nothing Nothing []
    Concurrent.threadDelay (2*1000*1000)
    heartbeat chan

insertLookup :: ChanMap -> T.Text -> IO ChanEvent
insertLookup cm@(ChanMap ref) k = do
    chan <- Chan.newChan
    tidTimeout <- Concurrent.forkIO $ setTimeout cm k
    tidHeartbeat <- Concurrent.forkIO $ heartbeat chan
    mChan <- IORef.atomicModifyIORef ref (ins chan [tidTimeout, tidHeartbeat])
    case mChan of
        -- chan already exists.
        (Just chan') -> return $ fst chan'
        -- inserted chan
        Nothing -> return chan
 where
     ins chan tid map_ =
         let (mValue, map') = Map.insertLookupWithKey oldValue k (chan, tid) map_
         in (map', mValue)
     oldValue _ _ old = old

writeChan :: ChanMap -> T.Text -> EventSource.ServerEvent -> IO ()
writeChan (ChanMap ref) k EventSource.CloseEvent = do
    maybeChan <- IORef.atomicModifyIORef ref modify
    case maybeChan of
        Just chan -> do
            mapM_ Concurrent.killThread (snd chan)
            -- send an empty 2 times to force flush
            Chan.writeChan (fst chan) $ EventSource.ServerEvent Nothing Nothing []
            Chan.writeChan (fst chan) $ EventSource.ServerEvent Nothing Nothing []
            Chan.writeChan (fst chan) EventSource.CloseEvent
            Mem.performGC
        Nothing -> return ()
 where
     modify map_ =
         case Map.updateLookupWithKey (\_ -> \_ -> Nothing) k map_ of
             (Nothing, _) -> (map_, Nothing)
             (mValue, map') -> (map', mValue)

writeChan (ChanMap ref) k event = do
    maybeChan <- Map.lookup k <$> IORef.readIORef ref
    case maybeChan of
        Just chan -> Chan.writeChan (fst chan) event
        Nothing -> Mem.performGC
