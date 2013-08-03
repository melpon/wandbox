module Handler.Root (
  makeRootR,
  getRootR
) where

import Import
import qualified Data.Text as T
import Codec.Binary.Url (encode)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import Control.Exception (bracket)
import System.IO (hClose, hFlush)

import qualified Data.Conduit as C
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)
import qualified Data.List (init)
import qualified Data.Aeson as JS (Value(..), object, (.=))
import Data.Aeson ((.=))

data CompilerSwitch = CompilerSwitch
  { swName :: Text
  , swFlags :: Text
  , swDefault :: Bool
  , swDisplayName :: Text
  }

data CompilerVersion = CompilerVersion
  { verName :: Text
  , verLanguage :: Text
  , verDisplayName :: Text
  , verVersion :: Text
  , verCompileCommand :: Text
  }

data CompilerInfo = CompilerInfo
  { ciVersion :: CompilerVersion
  , ciSwitches :: [CompilerSwitch]
  }

getCompilerInfos :: IO [CompilerInfo]
getCompilerInfos = do
    text <- fromVM
    let ls = Data.List.init $ T.split (=='\n') text
    let datas = map (T.split (==',')) ls
    return $ map makeCompilerInfo  datas
  where
    fromVM = do
      bracket connectVM hClose $ \handle -> do
        C.runResourceT $ CL.sourceList [Protocol Version ""] $$ sendVM handle
        hFlush handle
        C.runResourceT $ receiveVM handle $$ do
            result <- C.await
            case result of
                Nothing -> fail "failed: get version"
                (Just (Left err)) -> fail err
                (Just (Right (Protocol VersionResult version))) -> return version
                (Just (Right _)) -> fail $ "pattern is not match"
    makeCompilerInfo xs =
        CompilerInfo version switches
      where
        version = CompilerVersion (xs !! 0) (xs !! 1) (xs !! 2) (xs !! 3) (xs !! 4)
        switches = map makeSwitch $ drop 5 xs
    makeSwitch tsv =
        CompilerSwitch (xs !! 0) (xs !! 1) ((xs !! 2) == "true") (xs !! 3)
      where
        xs = T.split (=='\t') tsv

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
