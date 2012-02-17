module Handler.Root where

import Import
import Yesod.Static
import Network.Wai.EventSource

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Wandbox"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
        addStylesheet $ StaticR $ StaticRoute ["bootstrap", "css", "bootstrap.min.css"] []
        $(widgetFile "homepage")

