module Handler.Root (
  makeRootR,
  getRootR
) where

import Import
import qualified Data.Text as T
import Codec.Binary.Url (encode)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Exception (bracket)
import System.IO (hClose, hFlush)

import qualified Data.Conduit as C
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)
import qualified Data.List (init)
import qualified Data.Aeson as JS (Value(..), object, eitherDecode)
import qualified Data.Aeson.Types as JS (Parser)
import Data.Aeson ((.=), (.:))
import Control.Monad (MonadPlus(mzero), join)
import qualified Data.Attoparsec as A
import qualified Data.Text.Encoding as TE

data CompilerSwitchSelectOption = CompilerSwitchSelectOption
  { swmoName :: Text
  , swmoDisplayName :: Text
  , swmoDisplayFlags :: Text
  } deriving (Show)
instance FromJSON CompilerSwitchSelectOption where
  parseJSON (Object v) =
    CompilerSwitchSelectOption <$>
      v .: "name" <*>
      v .: "display-name" <*>
      v .: "display-flags"
  parseJSON _ = mzero

data CompilerSwitch =
  CompilerSwitchSingle
  { swsName :: Text
  , swsFlags :: Text
  , swsDefault :: Bool
  , swsDisplayName :: Text
  } |
  CompilerSwitchSelect
  { swmDefault :: Text
  , swmOptions :: [CompilerSwitchSelectOption]
  }
  deriving (Show)

instance FromJSON CompilerSwitch where
  parseJSON (Object v) = do
    typ <- v .: "type" :: JS.Parser String
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
  parseJSON _ = mzero

data CompilerVersion = CompilerVersion
  { verName :: Text
  , verLanguage :: Text
  , verDisplayName :: Text
  , verVersion :: Text
  , verCompileCommand :: Text
  } deriving (Show)
instance FromJSON CompilerVersion where
  parseJSON (Object v) =
    CompilerVersion <$>
      v .: "name" <*>
      v .: "language" <*>
      v .: "display-name" <*>
      v .: "version" <*>
      v .: "display-compile-command"
  parseJSON _ = mzero

data CompilerInfo = CompilerInfo
  { ciVersion :: CompilerVersion
  , ciSwitches :: [CompilerSwitch]
  } deriving (Show)

instance FromJSON CompilerInfo where
  parseJSON json@(Object v) = do
    version <- parseJSON json :: JS.Parser CompilerVersion
    switches <- join (parseJSON <$> (v .: "switches")) :: JS.Parser [CompilerSwitch]
    return $ CompilerInfo version switches
  parseJSON _ = mzero

getCompilerInfos :: IO [CompilerInfo]
getCompilerInfos = do
    text <- fromVM
    (Right infos) <- return $ JS.eitherDecode $ BL.pack $ B.unpack $ TE.encodeUtf8 text :: IO (Either String [CompilerInfo])
    return infos
  where
    fromVM = do
      bracket connectVM hClose $ \handle -> do
        C.runResourceT $ CL.sourceList [Protocol Version ""] $$ sendVM handle
        hFlush handle
        C.runResourceT $ receiveVM handle $$ do
            _ <- C.await
            result2 <- C.await
            case result2 of
                Nothing -> fail "failed: get version"
                (Just (Left err)) -> fail err
                (Just (Right (Protocol VersionResult version))) -> return version
                (Just (Right (Protocol VersionResult2 version))) -> return version
                (Just (Right _)) -> fail $ "pattern is not match"

resultContainer :: Widget
resultContainer = do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
  $(widgetFile "result_container")

resultWindow :: Widget
resultWindow = do
  addScriptRemote "//platform.twitter.com/widgets.js"
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
  addScript $ StaticR polyfills_EventSource_js
  $(widgetFile "result_window")

editor :: Widget
editor = do
  $(widgetFile "editor")

compiler :: Widget
compiler = do
  compilerInfos <- liftIO getCompilerInfos
  $(widgetFile "compiler")
  where
    makeSwitch _ (CompilerSwitchSingle name flags default_ displayName) =
      [whamlet|
        <div .row-fluid>
          <label .checkbox>
            $if default_
              <input type="checkbox" value="#{name}" flags="#{flags}" checked>#{displayName}
            $else
              <input type="checkbox" value="#{name}" flags="#{flags}">#{displayName}
      |]
    makeSwitch compilerName (CompilerSwitchSelect default_ options) = do
      let groupName = compilerName `mappend` default_
      [whamlet|
        $forall option <- options
          <label .radio>
            ^{makeSwitchSelectOption groupName default_ option}
      |]
    makeSwitchSelectOption groupName default_ (CompilerSwitchSelectOption name displayName displayFlags) =
      [whamlet|
        $if default_ == name
          <input type="radio" name="compile-option-groups-#{groupName}" value="#{name}" flags="#{displayFlags}" checked>#{displayName}
        $else
          <input type="radio" name="compile-option-groups-#{groupName}" value="#{name}" flags="#{displayFlags}">#{displayName}
      |]

makeRootR :: Maybe Code -> Handler Html
makeRootR mCode = do
    app <- getYesod
    defaultLayout $ do
        setTitle "[Wandbox]三へ( へ՞ਊ ՞)へ ﾊｯﾊｯ"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
        addScriptRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
        addScript $ StaticR js_jquery_url_js
        addScript $ StaticR ace_ace_js
        addScript $ StaticR ace_keybinding_vim_js
        addScript $ StaticR ace_keybinding_emacs_js
        addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
        resultWindow
        $(widgetFile "homepage")
  where
    urlEncode = JS.String . T.pack . encode . B.unpack . encodeUtf8
    defaultCompiler = JS.String "gcc-head"
    jsonCode = maybe JS.Null tojson mCode
    tojson code = JS.object ["compiler" .= urlEncode (codeCompiler code)
                            ,"code" .= urlEncode (codeCode code)
                            ,"options" .= urlEncode (codeOptions code)]

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler Html
getRootR = do
  makeRootR Nothing
