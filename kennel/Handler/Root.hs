module Handler.Root (
  makeRootR,
  getRootR
) where

import Import
import System.Random (randomRIO)
import qualified Data.Text as T
import Control.Monad (replicateM)
import Settings.StaticFiles (js_jquery_url_js, ace_ace_js, ace_keybinding_vim_js, ace_keybinding_emacs_js)
import Model
import Text.Julius (ToJavascript(toJavascript))
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

instance ToJavascript Bool where
  toJavascript True = "true"
  toJavascript False = "false"

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
    let ls = init $ T.split (=='\n') text
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

makeRootR :: Code -> Handler RepHtml
makeRootR code = do
    app <- getYesod
    defaultLayout $ do
        setTitle "[Wandbox]三へ( へ՞ਊ ՞)へ ﾊｯﾊｯ"
        -- sourceId <- liftIO $ T.pack <$> (replicateM 16 $ randomRIO ('a','z'))
        empty <- return T.empty
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
        addScript $ StaticR js_jquery_url_js
        addScript $ StaticR ace_ace_js
        addScript $ StaticR ace_keybinding_vim_js
        addScript $ StaticR ace_keybinding_emacs_js
        addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.no-icons.min.css"
        -- addStylesheet $ StaticR $ StaticRoute ["bootstrap", "css", "bootstrap.min.css"] []
        compilerInfos <- liftIO getCompilerInfos
        $(widgetFile "homepage")
  where
    urlEncode = encode . B.unpack . encodeUtf8


-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  emptyCode <- liftIO $ makeCode "gcc-head" "" ""
  makeRootR emptyCode
