module Api (
  CompilerSwitchSelectOption(..)
, CompilerSwitch(..)
, CompilerVersion(..)
, CompilerInfo(..)
, getCompilerInfos
, runCode
, sinkEventSource
, sinkProtocols
, sinkJson
, saveForPermlink
, getPermlink
) where

import Import

import qualified Blaze.ByteString.Builder.ByteString    as Blaze
import qualified Codec.Binary.Url                       as Url
import qualified Control.Exception                      as Exc
import qualified Control.Monad                          as Monad
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BSC
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Char                              as Char
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.List                      as ConduitL
import qualified Data.HashMap.Strict                    as HMS
import qualified Data.Maybe                             as Maybe
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE
import qualified Data.Time                              as Time
import qualified Data.Time.Clock                        as Clock
import qualified Network                                as N
import qualified Network.Wai.EventSource                as EventSource
import qualified System.IO                              as I
import qualified System.Locale                          as Locale
import qualified System.Random                          as Random
import qualified Yesod                                  as Y

import Data.Conduit (($$), ($=))
import Data.Aeson ((.=))

import Model (Code(..), LinkOutput(..), Link(..))
import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)
import Foundation (Handler)

import ApiTypes

getCompilerInfos :: N.HostName -> N.PortID -> IO [CompilerInfo]
getCompilerInfos host port = do
    text <- fromVM
    (Right infos) <- return $ Aeson.eitherDecode $ BSL.pack $ BS.unpack $ TE.encodeUtf8 text :: IO (Either String [CompilerInfo])
    return infos
  where
    fromVM = do
      Exc.bracket (connectVM host port) I.hClose $ \handle -> do
        Conduit.runResourceT $ ConduitL.sourceList [Protocol Version ""] $$ sendVM handle
        I.hFlush handle
        Conduit.runResourceT $ receiveVM handle $$ do
            result <- Conduit.await
            case result of
                Nothing -> fail "failed: get version"
                (Just (Left err)) -> fail err
                (Just (Right (Protocol VersionResult version))) -> return version
                (Just (Right _)) -> fail $ "pattern is not match"



makeProtocols :: Code -> [Protocol]
makeProtocols code =
  Maybe.catMaybes [
    Just $ Protocol Control (T.append "compiler=" $ codeCompiler code),
    Just $ Protocol StdIn $ codeStdin code,
    Just $ Protocol CompilerOptionRaw $ codeCompilerOptionRaw code,
    Just $ Protocol RuntimeOptionRaw $ codeRuntimeOptionRaw code,
    Just $ Protocol Source $ codeCode code,
    Just $ Protocol CompilerOption $ codeOptions code,
    Just $ Protocol Control "run"]

runCode :: N.HostName -> N.PortID -> Code -> Conduit.Sink (Either String Protocol) (Conduit.ResourceT IO) a -> IO a
runCode host port code sink =
  Exc.bracket (connectVM host port) I.hClose $ \handle -> do
    Conduit.runResourceT $ ConduitL.sourceList (makeProtocols code) $$ sendVM handle
    I.hFlush handle
    Conduit.runResourceT $ receiveVM handle $$ sink

urlEncode :: ProtocolSpecifier -> T.Text -> BS.ByteString
urlEncode spec contents = BS.concat [BSC.pack $ show spec, ":", BSC.pack $ Url.encode $ BS.unpack $ TE.encodeUtf8 contents]

sinkEventSource :: Conduit.MonadResource m => (EventSource.ServerEvent -> IO ()) -> Conduit.Sink (Either String Protocol) m ()
sinkEventSource writeChan_ = do
  mValue <- Conduit.await
  case mValue of
    Nothing -> return ()
    (Just (Left _)) -> do
      Y.liftIO $ writeChan_ $ EventSource.CloseEvent
    (Just (Right ProtocolNil)) -> do
      Y.liftIO $ print ProtocolNil
    (Just (Right (Protocol spec@Control contents@"Finish"))) -> do
      Y.liftIO $ writeChan_ $ EventSource.ServerEvent Nothing Nothing [Blaze.fromByteString $ urlEncode spec contents]
      Y.liftIO $ writeChan_ $ EventSource.CloseEvent
    (Just (Right (Protocol spec contents))) -> do
      Y.liftIO $ writeChan_ $ EventSource.ServerEvent Nothing Nothing [Blaze.fromByteString $ urlEncode spec contents]
      sinkEventSource writeChan_

sinkProtocols :: Conduit.MonadResource m => Conduit.Sink (Either String Protocol) m [Either String Protocol]
sinkProtocols = do
    protocols <- ConduitL.consume
    return protocols

sinkJson :: Conduit.MonadResource m => Conduit.Sink (Either String Protocol) m Aeson.Value
sinkJson = do
    xs <- sinkJson' []
    let m = HMS.fromListWith T.append xs
    let m' = HMS.map Y.String m
    return $ Y.Object m'
  where
    sinkJson' :: Conduit.MonadResource m => [(T.Text, T.Text)] -> Conduit.Sink (Either String Protocol) m [(T.Text, T.Text)]
    sinkJson' xs = do
      mValue <- Conduit.await
      case mValue of
        Nothing ->
          return $ ("error", "data is nothing"):xs
        (Just (Left str)) ->
          return $ ("error", T.pack str):xs
        (Just (Right ProtocolNil)) ->
          return xs
        (Just (Right (Protocol Control "Start"))) ->
          sinkJson' xs
        (Just (Right (Protocol Control "Finish"))) ->
          return xs
        (Just (Right (Protocol spec contents))) -> do
          let keys = specToKeys spec
          sinkJson' $ appendR (map (\key -> (key,contents)) keys) xs

    appendR (x:xs) ys = appendR xs (x:ys)
    appendR []     ys = ys
    specToKeys CompilerMessageE = ["compiler_error", "compiler_message"]
    specToKeys CompilerMessageS = ["compiler_output", "compiler_message"]
    specToKeys StdOut           = ["program_output", "program_message"]
    specToKeys StdErr           = ["program_error", "program_message"]
    specToKeys ExitCode         = ["status"]
    specToKeys Signal           = ["signal"]
    specToKeys _                = ["error"]

outputToProtocol :: LinkOutput -> Either String Protocol
outputToProtocol (LinkOutput _ _ typ output) = Right $ Protocol (read (T.unpack typ) :: ProtocolSpecifier) output

codeToJson :: Code -> Aeson.Value
codeToJson code =
    Aeson.object
      [ "compiler" .= codeCompiler code
      , "code" .= codeCode code
      , "options" .= codeOptions code
      , "compiler-option-raw" .= codeCompilerOptionRaw code
      , "runtime-option-raw" .= codeRuntimeOptionRaw code
      , "stdin" .= codeStdin code
      , "created-at" .= formatISO8601 (codeCreatedAt code)
      ]

formatISO8601 :: Clock.UTCTime -> String
formatISO8601 t = Time.formatTime Locale.defaultTimeLocale "%FT%T%QZ" t

saveForPermlink :: Code -> [Protocol] -> Handler T.Text
saveForPermlink code protocols = do
    linkName <- Y.liftIO $ T.pack <$> (Monad.replicateM 16 $ Char.chr <$> randomRAny (0,255) isLinkCode)
    Y.runDB $ do
        codeId <- Y.insert code
        let link = Link linkName codeId
        linkId <- Y.insert link
        let outputs = Maybe.mapMaybe (toOutput linkId) $ zip [0..] protocols
        _ <- Y.insertMany outputs
        return ()
    return linkName
  where
    toOutput linkId (order, (Protocol spec contents)) = Just $ LinkOutput linkId order (T.pack $ show spec) contents
    toOutput _ _ = Nothing
    randomRAny range p = do
      v <- Random.randomRIO range
      if p v then return v
                else randomRAny range p
    isLinkCode n | 48 <= n && n <= 57 = True -- 0-9
    isLinkCode n | 65 <= n && n <= 90 = True -- A-Z
    isLinkCode n | 97 <= n && n <= 122 = True -- a-z
    isLinkCode _ = False


getPermlink :: Conduit.MonadResource m => Code -> [LinkOutput] -> m Aeson.Value
getPermlink code outputs = do
    let codeJson = codeToJson code
    outputsJson <- ConduitL.sourceList outputs $= ConduitL.map outputToProtocol $$ sinkJson
    return $ Aeson.object
      [ "parameter" .= codeJson
      , "result" .= outputsJson
      ]
