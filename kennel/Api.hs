module Api (
  CompilerSwitchSelectOption(..)
, CompilerSwitch(..)
, CompilerVersion(..)
, CompilerInfo(..)
, getCompilerInfos
, runCode
, sinkEventSource
, sinkJson
) where

import Import

import qualified Blaze.ByteString.Builder.ByteString    as Blaze
import qualified Codec.Binary.Url                       as Url
import qualified Control.Exception                      as Exc
import qualified Control.Monad                          as Monad
import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Types                       as AesonTypes
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BSC
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.List                      as ConduitL
import qualified Data.HashMap.Strict                    as HMS
import qualified Data.Maybe                             as Maybe
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE
import qualified Network                                as N
import qualified Network.Wai.EventSource                as EventSource
import qualified System.IO                              as I
import qualified Yesod                                  as Y

import Data.Conduit (($$))
import Data.Aeson ((.:), (.=))

import Model (Code, codeCompiler, codeCode, codeOptions)
import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

data CompilerSwitchSelectOption = CompilerSwitchSelectOption
  { swmoName :: T.Text
  , swmoDisplayName :: T.Text
  , swmoDisplayFlags :: T.Text
  } deriving (Show)
instance Aeson.FromJSON CompilerSwitchSelectOption where
  parseJSON (Aeson.Object v) =
    CompilerSwitchSelectOption <$>
      v .: "name" <*>
      v .: "display-name" <*>
      v .: "display-flags"
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerSwitchSelectOption where
  toJSON (CompilerSwitchSelectOption name displayName displayFlags) =
    Aeson.object
      [ "name" .= name
      , "display-name" .= displayName
      , "display-flags" .= displayFlags
      ]

data CompilerSwitch =
  CompilerSwitchSingle
  { swsName :: T.Text
  , swsFlags :: T.Text
  , swsDefault :: Bool
  , swsDisplayName :: T.Text
  } |
  CompilerSwitchSelect
  { swmDefault :: T.Text
  , swmOptions :: [CompilerSwitchSelectOption]
  }
  deriving (Show)

instance Aeson.FromJSON CompilerSwitch where
  parseJSON (Aeson.Object v) = do
    typ <- v .: "type" :: AesonTypes.Parser String
    if typ == "single"
      then
        CompilerSwitchSingle <$>
          v .: "name" <*>
          v .: "display-flags" <*>
          v .: "default" <*>
          v .: "display-name"
      else
        CompilerSwitchSelect <$>
          v .: "default" <*>
          v .: "options"
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerSwitch where
  toJSON (CompilerSwitchSingle name flags def displayName) =
    Aeson.object
      [ "name" .= name
      , "display-flags" .= flags
      , "default" .= def
      , "display-name" .= displayName
      ]
  toJSON (CompilerSwitchSelect def options) =
    Aeson.object
      [ "default" .= def
      , "options" .= options
      ]

data CompilerVersion = CompilerVersion
  { verName :: T.Text
  , verLanguage :: T.Text
  , verDisplayName :: T.Text
  , verVersion :: T.Text
  , verCompileCommand :: T.Text
  } deriving (Show)
instance Aeson.FromJSON CompilerVersion where
  parseJSON (Aeson.Object v) =
    CompilerVersion <$>
      v .: "name" <*>
      v .: "language" <*>
      v .: "display-name" <*>
      v .: "version" <*>
      v .: "display-compile-command"
  parseJSON _ = Monad.mzero

data CompilerInfo = CompilerInfo
  { ciVersion :: CompilerVersion
  , ciSwitches :: [CompilerSwitch]
  } deriving (Show)

instance Aeson.FromJSON CompilerInfo where
  parseJSON json@(Aeson.Object v) = do
    version <- Aeson.parseJSON json :: AesonTypes.Parser CompilerVersion
    switches <- Monad.join (Aeson.parseJSON <$> (v .: "switches")) :: AesonTypes.Parser [CompilerSwitch]
    return $ CompilerInfo version switches
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerInfo where
  toJSON (CompilerInfo version switch) =
    Aeson.object
      [ "name" .= verName version
      , "language" .= verLanguage version
      , "display-name" .= verDisplayName version
      , "version" .= verVersion version
      , "display-compile-command" .= verCompileCommand version
      , "switches" .= switch
      ]
    

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
      sinkEventSource writeChan_

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
