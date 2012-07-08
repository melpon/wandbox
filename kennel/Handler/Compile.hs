module Handler.Compile (
  postCompileR
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

import Model (Code, codeCompiler, codeCode, codeOptimize, codeWarning, makeCode)
import qualified Data.Conduit as C
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

makeProtocols :: Code -> [Protocol]
makeProtocols code =
  catMaybes [Just $ Protocol Control (T.append "compiler=" $ codeCompiler code),
             Just $ Protocol Source $ codeCode code,
             Protocol CompilerOption <$> joinm (catMaybes [ifm (codeOptimize code) "optimize", ifm (codeWarning code) "warning"]),
             Just $ Protocol Control "run"]
  where
    joinm [] = Nothing
    joinm xs = Just $ T.pack $ joinm' xs
      where joinm' (y:ys) = y ++ "," ++ joinm' ys
            joinm' [] = ""
    ifm True x = Just x
    ifm False _ = Nothing

vmHandle :: Code -> C.Sink (Either String Protocol) (C.ResourceT IO) () -> IO ()
vmHandle code sink =
  bracket connectVM hClose $ \handle -> do
    C.runResourceT $ CL.sourceList (makeProtocols code) $$ sendVM handle
    hFlush handle
    C.runResourceT $ receiveVM handle $$ sink

urlEncode :: ProtocolSpecifier -> T.Text -> B.ByteString
urlEncode spec contents = B.concat [BC.pack $ show spec, ":", BC.pack $ encode $ B.unpack $ encodeUtf8 contents]

sinkProtocol :: C.MonadResource m => (ServerEvent -> IO ()) -> C.Sink (Either String Protocol) m ()
sinkProtocol writeChan = C.sinkState () push close
  where
    push _ (Left str) = do liftIO $ putStrLn str
                           return $ C.StateDone Nothing ()
    push _ (Right ProtocolNil) = do liftIO $ print ProtocolNil
                                    return $ C.StateDone Nothing ()
    push _ (Right (Protocol spec contents)) = do
      liftIO $ writeChan $ ServerEvent Nothing Nothing [fromByteString $ urlEncode spec contents]
      return $ C.StateProcessing ()
    close _ = return ()

postCompileR :: Text -> Handler ()
postCompileR ident = do
  mCompiler <- lookupPostParam "compiler"
  mCode <- lookupPostParam "code"
  mOpt <- lookupPostParam "optimize"
  mWarn <- lookupPostParam "warning"
  -- liftIO . (Just <$>) :: Maybe (IO Code) -> Handler (Maybe Code)
  mCodeInstance <- maybe (return Nothing) (liftIO . (Just <$>)) $
                       -- Maybe (IO Code)
                       makeCode <$> mCompiler
                                <*> mCode
                                <*> (bool <$> mOpt)
                                <*> (bool <$> mWarn)
  _ <- go mCodeInstance
  return ()
  where
    go (Just code) = do
      cm <- getChanMap <$> getYesod
      _ <- liftIO $ forkIO $ vmHandle code $ sinkProtocol $ CM.writeChan cm ident
      return ()
    go _ = return ()
    bool = (=="true")
