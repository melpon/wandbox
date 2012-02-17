module ChanMap (
  ChanMap,
  newChanMap,
  insert,
  delete,
  writeChan
) where

import Prelude hiding (lookup,catch)
import Control.Exception (IOException,catch)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Network.Wai.EventSource (ServerEvent(CommentEvent))
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative ((<$>))
import Control.Monad (forever, mapM_)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import qualified Control.Concurrent.Chan as C
import qualified Data.Map as M
import qualified Data.Text as T

type ChanEvent = C.Chan ServerEvent
data ChanMap = ChanMap (IORef (M.Map T.Text ChanEvent))

logging :: ChanMap -> IO ()
logging (ChanMap ref) = forever $ do
  threadDelay (15*1000*1000)
  print "---- remain ----"
  readIORef ref >>= mapM_ print . map fst . M.toList
  print "----------------"

newChanMap :: IO ChanMap
newChanMap = do
  cm <- ChanMap <$> newIORef M.empty
  --forkIO $ logging cm
  return cm

checkAlive :: ChanMap -> T.Text -> IO ()
checkAlive cm k = forever $ sendComment `catch` \e -> print (show (e :: IOException)) >> delete cm k
  where
    sendComment = do
      threadDelay (15*1000*1000)
      writeChan cm k $ CommentEvent $ fromString "checkAlive"

insert :: ChanMap -> T.Text -> IO ChanEvent
insert cm@(ChanMap ref) k = do
  chan <- C.newChan
  _ <- atomicModifyIORef ref $ \m -> (M.insert k chan m, ())
  --forkIO $ checkAlive cm k
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

