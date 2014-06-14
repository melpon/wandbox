module Handler.Source (
  getSourceR
, getEmptySourceR
) where

import Import
import qualified Network.Wai.EventSource                as EventSource
import qualified Data.Text                              as T
import qualified Yesod                                  as Y

import ChanMap (insertLookup)
import Foundation (Handler, getChanMap)

getSourceR :: T.Text -> Handler ()
getSourceR ident = do
    cm <- getChanMap <$> Y.getYesod
    chan <- Y.liftIO $ insertLookup cm ident

    req <- Y.waiRequest
    res <- Y.liftIO $ EventSource.eventSourceAppChan chan req

    Y.addHeader "Pragma" "no-chache"
    Y.addHeader "Cache-Control" "no-chache"

    Y.sendWaiResponse res

getEmptySourceR :: Handler ()
getEmptySourceR = Y.notFound
