module Handler.Compile where

import Import
import Network.Wai.EventSource (ServerEvent(..), eventSourceApp)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, join, replicateM, forM_)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import System.Random (randomRIO)
import qualified Data.Text as T
import qualified ChanMap as CM

getSourceR :: Handler ()
getSourceR = do
    chan <- liftIO newChan
    ident <- liftIO $ T.pack <$> (replicateM 16 $ randomRIO ('a','z'))
    cm <- getChanMap <$> getYesod
    chan <- liftIO $ CM.insert cm ident

    req <- waiRequest
    res <- lift $ eventSourceApp chan req

    _ <- liftIO $ CM.writeChan cm ident $ ServerEvent Nothing Nothing [fromText ident]

    sendWaiResponse res

postCompileR :: Text -> Handler ()
postCompileR ident = do
    cm <- getChanMap <$> getYesod

    codes <- T.lines <$> maybe undefined id <$> lookupPostParam "code"
    _ <- liftIO $ forkIO $ do
            forM_ codes $ \code -> do
                                   CM.writeChan cm ident $ ServerEvent Nothing Nothing [fromText code]
                                   threadDelay (1000*1000)
            CM.delete cm ident

    return ()
