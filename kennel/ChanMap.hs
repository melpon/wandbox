module ChanMap (
  ChanMap,
  newChanMap,
  insertLookup,
  writeChan
) where

import Prelude
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Network.Wai.EventSource (ServerEvent(..))
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Applicative ((<$>))
import Control.Exception (finally)
import qualified Control.Concurrent.Chan as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Mem as Mem

type ChanEvent = C.Chan ServerEvent
data ChanMap = ChanMap (IORef (M.Map T.Text (ChanEvent, [ThreadId])))

newChanMap :: IO ChanMap
newChanMap = do
  cm <- ChanMap <$> newIORef M.empty
  return cm

setTimeout :: ChanMap -> T.Text -> IO ()
setTimeout cm k = do
    -- wait 2 minutes
    -- FIXME: This timeout should be elonged by timing of any data transmitted.
    threadDelay (2*60*1000*1000)
    `finally`
    writeChan cm k CloseEvent

heartbeat :: ChanEvent -> IO ()
heartbeat chan = do
    -- send an empty 2 times to force flush
    C.writeChan chan $ ServerEvent Nothing Nothing []
    C.writeChan chan $ ServerEvent Nothing Nothing []
    threadDelay (2*1000*1000)
    heartbeat chan

insertLookup :: ChanMap -> T.Text -> IO ChanEvent
insertLookup cm@(ChanMap ref) k = do
    chan <- C.newChan
    tidTimeout <- forkIO $ setTimeout cm k
    tidHeartbeat <- forkIO $ heartbeat chan
    mChan <- atomicModifyIORef ref (ins chan [tidTimeout, tidHeartbeat])
    case mChan of
        -- chan already exists.
        (Just chan') -> return $ fst chan'
        -- inserted chan
        Nothing -> return chan
 where
     ins chan tid map_ =
         let (mValue, map') = M.insertLookupWithKey oldValue k (chan, tid) map_
         in (map', mValue)
     oldValue _ _ old = old

writeChan :: ChanMap -> T.Text -> ServerEvent -> IO ()
writeChan (ChanMap ref) k CloseEvent = do
    maybeChan <- atomicModifyIORef ref modify
    case maybeChan of
        Just chan -> do
            mapM_ killThread (snd chan)
            -- send an empty 2 times to force flush
            C.writeChan (fst chan) $ ServerEvent Nothing Nothing []
            C.writeChan (fst chan) $ ServerEvent Nothing Nothing []
            C.writeChan (fst chan) CloseEvent
            Mem.performGC
        Nothing -> return ()
 where
     modify map_ =
         case M.updateLookupWithKey (\_ -> \_ -> Nothing) k map_ of
             (Nothing, _) -> (map_, Nothing)
             (Just mValue, map') -> (map', Just mValue)

writeChan (ChanMap ref) k event = do
    maybeChan <- M.lookup k <$> readIORef ref
    case maybeChan of
        Just chan -> C.writeChan (fst chan) event
        Nothing -> Mem.performGC
