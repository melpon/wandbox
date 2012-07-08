module ChanMap (
  ChanMap,
  newChanMap,
  insertLookup,
  delete,
  writeChan
) where

import Prelude hiding (lookup,catch)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Network.Wai.EventSource (ServerEvent(..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative ((<$>))
import qualified Control.Concurrent.Chan as C
import qualified Data.Map as M
import qualified Data.Text as T

type ChanEvent = C.Chan ServerEvent
data ChanMap = ChanMap (IORef (M.Map T.Text ChanEvent))

newChanMap :: IO ChanMap
newChanMap = do
  cm <- ChanMap <$> newIORef M.empty
  return cm

timeDelete :: ChanMap -> T.Text -> IO ()
timeDelete cm k = do
  -- wait 2 hours
  threadDelay (2*60*60*1000*1000)
  writeChan cm k CloseEvent
  delete cm k

heartbeat :: ChanMap -> T.Text -> IO ()
heartbeat cm k = do
  threadDelay (10*1000*1000)
  chan <- lookup cm k
  C.writeChan chan $ ServerEvent Nothing Nothing []
  heartbeat cm k
  
insertLookup :: ChanMap -> T.Text -> IO ChanEvent
insertLookup cm@(ChanMap ref) k = do
  chan <- C.newChan
  chan' <- atomicModifyIORef ref (modify chan)
  -- this chan is removed from ChanMap after few seconds.
  _ <- forkIO $ timeDelete cm k
  -- heartbeat with CommentEvent.
  _ <- forkIO $ heartbeat cm k
  return chan'
  where
    modify chan m = let (mValue, m') = M.insertLookupWithKey oldValue k chan m
                    in maybe (m', chan) ((,) m') mValue
    oldValue _ _ old = old

delete :: ChanMap -> T.Text -> IO ()
delete (ChanMap ref) k = do
  atomicModifyIORef ref $ \m -> (M.delete k m, ())

lookup :: ChanMap -> T.Text -> IO ChanEvent
lookup (ChanMap ref) k = do
  m <- readIORef ref
  maybe (fail "key not found") return $ M.lookup k m

writeChan :: ChanMap -> T.Text -> ServerEvent -> IO ()
writeChan cm k event = do
  chan <- lookup cm k
  C.writeChan chan event

