module Handler.Root (
  makeRootR,
  getRootR
) where

import Import

import qualified Data.Text                              as T
import qualified Codec.Binary.Url                       as Url
import qualified Data.Text.Encoding                     as TE
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BSL
import qualified Control.Exception                      as Exc
import qualified Control.Exception.Lifted               as ExcL
import qualified System.IO                              as I
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.List                      as ConduitL
import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Types                       as AesonTypes
import qualified Control.Monad                          as Monad
import qualified Network                                as N
import qualified Network.HTTP.Types                     as HT
import qualified Yesod                                  as Y

import Data.Aeson ((.=), (.:))
import Data.Conduit (($$))
import Yesod (whamlet, shamlet)

import Model (Code(..), LinkOutput(..))
import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)
import Settings.StaticFiles (
    polyfills_EventSource_js, js_jquery_url_js,
    codemirror_lib_codemirror_js,
    codemirror_lib_codemirror_css,
    codemirror_mode_clike_clike_js,
    codemirror_mode_d_d_js,
    codemirror_mode_ruby_ruby_js,
    codemirror_mode_python_python_js,
    codemirror_mode_perl_perl_js,
    codemirror_mode_erlang_erlang_js,
    codemirror_mode_haskell_haskell_js,
    codemirror_mode_shell_shell_js,
    codemirror_keymap_vim_js,
    codemirror_keymap_emacs_js,
    compiling_gif)
import Settings (widgetFile, Extra(..))
import Foundation (Handler, Widget, Route(..), getExtra)


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

resultContainer :: Widget
resultContainer = do
  Y.addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
  $(widgetFile "result_container")

resultWindow :: Widget
resultWindow = do
  Y.addScriptRemote "//platform.twitter.com/widgets.js"
  Y.addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
  Y.addScript $ StaticR polyfills_EventSource_js
  $(widgetFile "result_window")

editor :: Widget
editor = do
  $(widgetFile "editor")

compiler :: Widget
compiler = do
  host <- Y.handlerToWidget $ extraVMHost <$> getExtra
  port <- Y.handlerToWidget $ extraVMPort <$> getExtra
  compilerInfos <- Y.liftIO (getCompilerInfos host port)
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
      let groupName = compilerName `T.append` default_
      [whamlet|
        <select .span12>
          $forall option <- options
            ^{makeSwitchSelectOption groupName default_ option}
      |]
    makeSwitchSelectOption groupName default_ (CompilerSwitchSelectOption name displayName displayFlags) =
      [whamlet|
        $if default_ == name
          <option name="compile-option-groups-#{groupName}" value="#{name}" flags="#{displayFlags}" selected>#{displayName}
        $else
          <option name="compile-option-groups-#{groupName}" value="#{name}" flags="#{displayFlags}">#{displayName}
      |]

makeRootR :: Maybe (Code, [LinkOutput]) -> Handler Y.Html
makeRootR mCodeOutputs = do
    app <- Y.getYesod
    Y.defaultLayout $ do
        Y.setTitle "[Wandbox]三へ( へ՞ਊ ՞)へ ﾊｯﾊｯ"
        Y.addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
        Y.addScriptRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
        Y.addScript $ StaticR js_jquery_url_js
        Y.addScript $ StaticR codemirror_lib_codemirror_js
        Y.addStylesheet $ StaticR codemirror_lib_codemirror_css
        Y.addScript $ StaticR codemirror_mode_clike_clike_js
        Y.addScript $ StaticR codemirror_mode_d_d_js
        Y.addScript $ StaticR codemirror_mode_ruby_ruby_js
        Y.addScript $ StaticR codemirror_mode_python_python_js
        Y.addScript $ StaticR codemirror_mode_perl_perl_js
        Y.addScript $ StaticR codemirror_mode_erlang_erlang_js
        Y.addScript $ StaticR codemirror_mode_haskell_haskell_js
        Y.addScript $ StaticR codemirror_mode_shell_shell_js
        Y.addScript $ StaticR codemirror_keymap_vim_js
        Y.addScript $ StaticR codemirror_keymap_emacs_js
        Y.addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
        resultWindow
        $(widgetFile "homepage")
  where
    urlEncode = Aeson.String . T.pack . Url.encode . BS.unpack . TE.encodeUtf8
    defaultCompiler = Aeson.String "gcc-head"
    jsonCode = maybe Aeson.Null tojson mCodeOutputs
    tojson (code, outputs) =
        Aeson.object ["compiler" .= urlEncode (codeCompiler code)
                     ,"code" .= urlEncode (codeCode code)
                     ,"options" .= urlEncode (codeOptions code)
                     ,"outputs" .= map tojson' outputs]
      where
        tojson' output =
            Aeson.object ["type" .= urlEncode (linkOutputType output)
                         ,"output" .= urlEncode (linkOutputOutput output)]

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler Y.Html
getRootR = do
  makeRootR Nothing `ExcL.catch` serviceUnavailable
  where
    serviceUnavailable :: ExcL.SomeException -> Handler Y.Html
    serviceUnavailable _ = Y.sendResponseStatus HT.status503 [shamlet|
      !!!
      <html>
        <head>
          <title>[Wandbox]三へ( へ՞ਊ ՞)へ ﾊｯﾊｯ
        <body>
          <h1>Service is Unavailable
          <div>
            <p>Cattleshed is down. Please contact #
              <a href="https://twitter.com/melponn">@melponn
              \ by a reply or a direct message.
    |]
