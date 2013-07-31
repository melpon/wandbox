module ChanMap (
  ChanMap,
  newChanMap,
  insertLookup,
  delete,
  writeChan
) where

import Prelude hiding (lookup)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Network.Wai.EventSource (ServerEvent(..))
import qualified Network.Wai.EventSource.EventStream as EventStream (eventToBuilder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative ((<$>))
import Control.Exception (finally)
import qualified Control.Concurrent.Chan as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.Int

type ChanEvent = C.Chan ServerEvent
type SendBytes = Data.Int.Int64
data ChanMap = ChanMap (IORef (M.Map T.Text (ChanEvent, SendBytes)))

newChanMap :: IO ChanMap
newChanMap = do
  cm <- ChanMap <$> newIORef M.empty
  return cm

timeDelete :: ChanMap -> T.Text -> IO ()
timeDelete cm k = do
    -- wait 2 minutes
    threadDelay (2*60*1000*1000)
    writeChan cm k CloseEvent
  `finally`
    delete cm k

heartbeat :: ChanMap -> T.Text -> IO ()
heartbeat cm k = do
  chan <- lookup cm k
  C.writeChan chan $ ServerEvent Nothing Nothing []
  C.writeChan chan $ ServerEvent Nothing Nothing []
  threadDelay (2*1000*1000)
  heartbeat cm k

insertLookup :: ChanMap -> T.Text -> IO ChanEvent
insertLookup cm@(ChanMap ref) k = do
  chan <- C.newChan
  mChan <- atomicModifyIORef ref (modify chan)
  case mChan of
    -- chan already exists.
    (Just chan') -> return chan'
    -- inserted chan
    Nothing -> do
      -- this chan is removed from ChanMap after few seconds.
      _ <- forkIO $ timeDelete cm k
      -- heartbeat with CommentEvent.
      _ <- forkIO $ heartbeat cm k
      return chan
  where
    modify chan map_ = let (mValue, map') = M.insertLookupWithKey oldValue k (chan, 0) map_
                    in (map', fst <$> mValue)
    oldValue _ _ old = old

delete :: ChanMap -> T.Text -> IO ()
delete (ChanMap ref) k = do
  atomicModifyIORef ref $ \map_ -> (M.delete k map_, ())

lookup :: ChanMap -> T.Text -> IO ChanEvent
lookup (ChanMap ref) k = do
  map_ <- readIORef ref
  maybe (fail "key not found") (return . fst) $ M.lookup k map_

writeChan :: ChanMap -> T.Text -> ServerEvent -> IO ()
writeChan (ChanMap ref) k event = do
  (Just (chan, sendSize)) <- atomicModifyIORef ref modify
  if sendSize >= 10 * 1024
    then fail "size that written a chan is too large."
    else C.writeChan chan event
  where
    modify map_ =
        case mResult of
          Nothing -> (map_, Nothing)
          (Just (mValue, map')) -> (map', mValue)
      where
        mEventSize = BSL.length <$> Blaze.toLazyByteString <$> EventStream.eventToBuilder event
        update eventSize = M.updateLookupWithKey (newValue eventSize) k map_
        newValue eventSize _ v = Just (fst v, snd v + eventSize)
        mResult = update <$> mEventSize
