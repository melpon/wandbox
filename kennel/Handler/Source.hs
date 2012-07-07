module Handler.Source (
  getSourceR
) where

import Import
import Network.Wai.EventSource (eventSourceAppChan)
import qualified ChanMap as CM

getSourceR :: Text -> Handler ()
getSourceR ident = do
    cm <- getChanMap <$> getYesod
    chan <- liftIO $ CM.insertLookup cm ident

    req <- waiRequest
    res <- lift $ eventSourceAppChan chan req

    sendWaiResponse res

