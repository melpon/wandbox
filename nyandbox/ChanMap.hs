module ChanMap (
  ChanMap,
  newChanMap,
  insert,
  delete,
  writeChan
) where

import Prelude hiding (lookup,catch)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Network.Wai.EventSource (ServerEvent)
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
  threadDelay (30*1000*1000)
  delete cm k

insert :: ChanMap -> T.Text -> IO ChanEvent
insert cm@(ChanMap ref) k = do
  chan <- C.newChan
  _ <- atomicModifyIORef ref $ \m -> (M.insert k chan m, ())
  -- this chan is removed from ChanMap after few seconds.
  _ <- forkIO $ timeDelete cm k
  return chan

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

