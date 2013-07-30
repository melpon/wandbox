module Handler.Source (
  getSourceR
, getEmptySourceR
) where

import Import
import Network.Wai.EventSource (eventSourceAppChan)
import qualified ChanMap as CM
import Data.Conduit (ResourceT, runResourceT)

getSourceR :: Text -> Handler ()
getSourceR ident = do
    cm <- getChanMap <$> getYesod
    chan <- liftIO $ CM.insertLookup cm ident

    req <- waiRequest
    res <- liftIO $ runResourceT $ eventSourceAppChan chan req

    sendWaiResponse res

getEmptySourceR :: Handler ()
getEmptySourceR = notFound
