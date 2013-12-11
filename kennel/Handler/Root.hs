module Handler.Root (
  makeRootR,
  getRootR
) where

import Import

import qualified Data.Text                              as T
import qualified Codec.Binary.Url                       as Url
import qualified Data.Text.Encoding                     as TE
import qualified Data.ByteString                        as BS
import qualified Control.Exception.Lifted               as ExcL
import qualified Data.Aeson                             as Aeson
import qualified Network.HTTP.Types                     as HT
import qualified Yesod                                  as Y

import Data.Aeson ((.=))
import Yesod (whamlet, shamlet)

import Model (Code(..), LinkOutput(..))
import Settings.StaticFiles (
    polyfills_EventSource_js,
    js_jquery_url_js,
    js_jquery_cookie_js,
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
    codemirror_mode_lua_lua_js,
    codemirror_mode_php_php_js,
    codemirror_keymap_vim_js,
    codemirror_keymap_emacs_js,
    compiling_gif)
import Settings (widgetFile, Extra(..))
import Foundation (Handler, Widget, Route(..), getExtra)
import Api
    ( getCompilerInfos
    , CompilerSwitchSelectOption(..)
    , CompilerSwitch(..)
    , CompilerVersion(..)
    , CompilerInfo(..)
    )


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
              <input type="checkbox" value="#{name}" data-flags="#{flags}" checked>#{displayName}
            $else
              <input type="checkbox" value="#{name}" data-flags="#{flags}">#{displayName}
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
          <option name="compile-option-groups-#{groupName}" value="#{name}" data-flags="#{displayFlags}" selected>#{displayName}
        $else
          <option name="compile-option-groups-#{groupName}" value="#{name}" data-flags="#{displayFlags}">#{displayName}
      |]

makeRootR :: Maybe (Code, [LinkOutput]) -> Handler Y.Html
makeRootR mCodeOutputs = do
    app <- Y.getYesod
    Y.defaultLayout $ do
        Y.setTitle "[Wandbox]三へ( へ՞ਊ ՞)へ ﾊｯﾊｯ"
        Y.addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
        Y.addScriptRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
        Y.addScript $ StaticR js_jquery_url_js
        Y.addScript $ StaticR js_jquery_cookie_js
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
        Y.addScript $ StaticR codemirror_mode_lua_lua_js
        Y.addScript $ StaticR codemirror_mode_php_php_js
        Y.addScript $ StaticR codemirror_keymap_vim_js
        Y.addScript $ StaticR codemirror_keymap_emacs_js
        Y.addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
        resultWindow
        $(widgetFile "homepage")
  where
    urlEncode = Aeson.String . T.pack . Url.encode . BS.unpack . TE.encodeUtf8
    defaultCompiler = Aeson.String "gcc-head"
    usingPermlink = maybe False (const True) mCodeOutputs
    jsonCode = maybe Aeson.Null tojson mCodeOutputs
    tojson (code, outputs) =
        Aeson.object ["compiler" .= urlEncode (codeCompiler code)
                     ,"code" .= urlEncode (codeCode code)
                     ,"options" .= urlEncode (codeOptions code)
                     ,"compiler-option-raw" .= urlEncode (codeCompilerOptionRaw code)
                     ,"runtime-option-raw" .= urlEncode (codeRuntimeOptionRaw code)
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
