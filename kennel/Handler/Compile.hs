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

import qualified Data.Conduit as C
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

data CompileData = CompileData
  { coCompiler :: T.Text
  , coCode :: T.Text
  , coOptimize :: Bool
  , coWarning :: Bool
  }

makeProtocols :: CompileData -> [Protocol]
makeProtocols cdata =
  catMaybes [Just $ Protocol Control (T.append "compiler=" $ coCompiler cdata),
             Just $ Protocol Source $ coCode cdata,
             Protocol CompilerOption <$> joinm (catMaybes [ifm (coOptimize cdata) "optimize", ifm (coWarning cdata) "warning"]),
             Just $ Protocol Control "run"]
  where
    joinm [] = Nothing
    joinm xs = Just $ T.pack $ joinm' xs
      where joinm' (y:ys) = y ++ "," ++ joinm' ys
            joinm' [] = ""
    ifm True x = Just x
    ifm False _ = Nothing

vmHandle :: CompileData -> C.Sink (Either String Protocol) (C.ResourceT IO) () -> IO ()
vmHandle cdata sink =
  bracket connectVM hClose $ \handle -> do
    C.runResourceT $ CL.sourceList (makeProtocols cdata) $$ sendVM handle
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
  _ <- go $ CompileData <$> mCompiler
                        <*> mCode
                        <*> (bool <$> mOpt)
                        <*> (bool <$> mWarn)
  return ()
  where
    go (Just cdata) = do
      cm <- getChanMap <$> getYesod
      _ <- liftIO $ forkIO $ vmHandle cdata $ sinkProtocol $ CM.writeChan cm ident
      return ()
    go _ = return ()
    bool = (=="true")
