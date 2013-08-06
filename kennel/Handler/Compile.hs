module Handler.Compile (
  postCompileR
, getEmptyCompileR
) where

import Import
import qualified Network.Wai.EventSource                as EventSource
import qualified Control.Concurrent                     as Concurrent
import qualified Blaze.ByteString.Builder.ByteString    as Blaze
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BSC
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE
import qualified Data.Maybe                             as Maybe
import qualified Control.Exception                      as Exc
import qualified System.IO                              as I
import qualified Codec.Binary.Url                       as Url
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.List                      as ConduitL
import qualified Yesod                                  as Y

import Data.Conduit (($$))

import Foundation (Handler, getChanMap)
import Model (makeCode, Code, codeCompiler, codeCode, codeOptions)
import ChanMap (writeChan)
import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

makeProtocols :: Code -> [Protocol]
makeProtocols code =
  Maybe.catMaybes [
    Just $ Protocol Control (T.append "compiler=" $ codeCompiler code),
    Just $ Protocol Source $ codeCode code,
    Just $ Protocol CompilerOption $ codeOptions code,
    Just $ Protocol Control "run"]

vmHandle :: Code -> Conduit.Sink (Either String Protocol) (Conduit.ResourceT IO) () -> IO ()
vmHandle code sink =
  Exc.bracket connectVM I.hClose $ \handle -> do
    Conduit.runResourceT $ ConduitL.sourceList (makeProtocols code) $$ sendVM handle
    I.hFlush handle
    Conduit.runResourceT $ receiveVM handle $$ sink

urlEncode :: ProtocolSpecifier -> T.Text -> BS.ByteString
urlEncode spec contents = BS.concat [BSC.pack $ show spec, ":", BSC.pack $ Url.encode $ BS.unpack $ TE.encodeUtf8 contents]

sinkProtocol :: Conduit.MonadResource m => (EventSource.ServerEvent -> IO ()) -> Conduit.Sink (Either String Protocol) m ()
sinkProtocol writeChan_ = do
  mValue <- Conduit.await
  case mValue of
    Nothing -> return ()
    (Just (Left str)) -> do
      Y.liftIO $ putStrLn str
      Y.liftIO $ writeChan_ $ EventSource.CloseEvent
    (Just (Right ProtocolNil)) -> do
      Y.liftIO $ print ProtocolNil
    (Just (Right (Protocol spec@Control contents@"Finish"))) -> do
      Y.liftIO $ putStrLn $ T.unpack contents
      Y.liftIO $ writeChan_ $ EventSource.ServerEvent Nothing Nothing [Blaze.fromByteString $ urlEncode spec contents]
      Y.liftIO $ writeChan_ $ EventSource.CloseEvent
    (Just (Right (Protocol spec contents))) -> do
      Y.liftIO $ writeChan_ $ EventSource.ServerEvent Nothing Nothing [Blaze.fromByteString $ urlEncode spec contents]
      sinkProtocol writeChan_

getEmptyCompileR :: Handler ()
getEmptyCompileR = Y.notFound

postCompileR :: T.Text -> Handler ()
postCompileR ident = do
  (Just compiler) <- Y.lookupPostParam "compiler"
  (Just code) <- Y.lookupPostParam "code"
  (Just options) <- Y.lookupPostParam "options"
  codeInstance <- Y.liftIO $ makeCode compiler code options
  _ <- go codeInstance
  return ()
  where
    go codeInstance = do
      cm <- getChanMap <$> Y.getYesod
      _ <- Y.liftIO $ Concurrent.forkIO $ vmHandle codeInstance $ sinkProtocol $ writeChan cm ident
      return ()
