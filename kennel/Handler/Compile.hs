module Handler.Compile (
  postCompileR
, getEmptyCompileR
) where

import Import
import Network.Wai.EventSource (ServerEvent(..))
import Control.Concurrent (forkIO)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified ChanMap as CM
import Data.Maybe (catMaybes)
import Control.Exception (bracket)
import System.IO (hClose, hFlush)
import Codec.Binary.Url (encode)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Conduit as C
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

makeProtocols :: Code -> [Protocol]
makeProtocols code =
  catMaybes [Just $ Protocol Control (T.append "compiler=" $ codeCompiler code),
             Just $ Protocol Source $ codeCode code,
             Just $ Protocol CompilerOption $ codeOptions code,
             Just $ Protocol Control "run"]

vmHandle :: Code -> C.Sink (Either String Protocol) (C.ResourceT IO) () -> IO ()
vmHandle code sink =
  bracket connectVM hClose $ \handle -> do
    C.runResourceT $ CL.sourceList (makeProtocols code) $$ sendVM handle
    hFlush handle
    C.runResourceT $ receiveVM handle $$ sink

urlEncode :: ProtocolSpecifier -> T.Text -> B.ByteString
urlEncode spec contents = B.concat [BC.pack $ show spec, ":", BC.pack $ encode $ B.unpack $ encodeUtf8 contents]

sinkProtocol :: C.MonadResource m => (ServerEvent -> IO ()) -> C.Sink (Either String Protocol) m ()
sinkProtocol writeChan = do
  mValue <- C.await
  case mValue of
    Nothing -> return ()
    (Just (Left str)) -> do
      liftIO $ putStrLn str
      liftIO $ writeChan $ CloseEvent
    (Just (Right ProtocolNil)) -> do
      liftIO $ print ProtocolNil
    (Just (Right (Protocol spec@Control contents@"Finish"))) -> do
      liftIO $ putStrLn $ T.unpack contents
      liftIO $ writeChan $ ServerEvent Nothing Nothing [fromByteString $ urlEncode spec contents]
      liftIO $ writeChan $ CloseEvent
    (Just (Right (Protocol spec contents))) -> do
      liftIO $ writeChan $ ServerEvent Nothing Nothing [fromByteString $ urlEncode spec contents]
      sinkProtocol writeChan

getEmptyCompileR :: Handler ()
getEmptyCompileR = notFound

postCompileR :: Text -> Handler ()
postCompileR ident = do
  (Just compiler) <- lookupPostParam "compiler"
  (Just code) <- lookupPostParam "code"
  (Just options) <- lookupPostParam "options"
  codeInstance <- liftIO $ makeCode compiler code options
  _ <- go codeInstance
  return ()
  where
    go codeInstance = do
      cm <- getChanMap <$> getYesod
      _ <- liftIO $ forkIO $ vmHandle codeInstance $ sinkProtocol $ CM.writeChan cm ident
      return ()
