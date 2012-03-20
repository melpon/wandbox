module Handler.Compile (
  getSourceR,
  postCompileR
) where

import Import
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Control.Concurrent (forkIO)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import qualified Data.Text as T
import qualified ChanMap as CM
import Control.Exception (bracket)
import System.IO (hClose, hFlush)

import qualified Data.Conduit as C
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..), toString)
import VM.Conduit (connectVM, sendVM, receiveVM)

getSourceR :: Text -> Handler ()
getSourceR ident = do
    cm <- getChanMap <$> getYesod
    chan <- liftIO $ CM.insertLookup cm ident

    req <- waiRequest
    res <- lift $ eventSourceAppChan chan req

    sendWaiResponse res

vmHandle :: T.Text -> C.Sink (Either String Protocol) IO () -> IO ()
vmHandle code sink =
  bracket connectVM hClose $ \handle -> do
    C.runResourceT $ CL.sourceList protos $$ sendVM handle
    hFlush handle
    C.runResourceT $ receiveVM handle $$ sink
  where
    protos = [Protocol Control "compiler=gcc",
              Protocol CompilerSwitch "<optimize>2 <address-model>64",
              Protocol Source code,
              Protocol Control "run"]

sinkProtocol :: (ServerEvent -> IO ()) -> C.Sink (Either String Protocol) IO ()
sinkProtocol writeChan = C.sinkIO (return ()) return push close
  where
    push _ (Left str) = do putStrLn str
                           return $ C.IODone Nothing ()
    push _ (Right ProtocolNil) = do print ProtocolNil
                                    return $ C.IODone Nothing ()
    push _ (Right proto) = do
      writeChan $ ServerEvent Nothing Nothing [fromByteString $ toString proto]
      return C.IOProcessing
    close _ = return ()

postCompileR :: Text -> Handler ()
postCompileR ident = do
  mCode <- lookupPostParam "code"
  maybe (return ()) go mCode
  where
    go code = do
      cm <- getChanMap <$> getYesod
      _ <- liftIO $ forkIO $ vmHandle code $ sinkProtocol $ CM.writeChan cm ident
      return ()
