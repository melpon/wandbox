module Handler.Root (
  makeRootR,
  getRootR
) where

import Import
import System.Random (randomRIO)
import qualified Data.Text as T
import Control.Monad (replicateM)
import Settings.StaticFiles (js_jquery_url_js)
import Model
import Text.Julius (ToJavascript(toJavascript))
import Codec.Binary.Url (encode)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B

instance ToJavascript Bool where
  toJavascript True = "true"
  toJavascript False = "false"

makeRootR :: Code -> Handler RepHtml
makeRootR code = do
    defaultLayout $ do
        setTitle "Wandbox"
        sourceId <- liftIO $ T.pack <$> (replicateM 16 $ randomRIO ('a','z'))
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
        addScript $ StaticR js_jquery_url_js
        addStylesheet $ StaticR $ StaticRoute ["bootstrap", "css", "bootstrap.min.css"] []
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
  emptyCode <- liftIO $ makeCode "gcc" "" False True
  makeRootR emptyCode
