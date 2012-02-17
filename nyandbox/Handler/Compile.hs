module Handler.Compile where

import Import
import Network.Wai.EventSource (ServerEvent(..), eventSourceApp)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, join, replicateM, forM_)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import qualified Data.Map as M
import Data.IORef (readIORef, modifyIORef)
import System.Random (randomRIO)
import qualified Data.Text as T

getSourceR :: Handler ()
getSourceR = do
    chan <- liftIO newChan
    ident <- liftIO $ T.pack <$> (replicateM 16 $ randomRIO ('a','z'))
    ref <- getChanMapRef <$> getYesod
    _ <- liftIO $ modifyIORef ref $ M.insert ident chan

    req <- waiRequest
    res <- lift $ eventSourceApp chan req

    _ <- liftIO $ writeChan chan $ ServerEvent Nothing Nothing [fromText ident]

    sendWaiResponse res

postCompileR :: Text -> Handler ()
postCompileR ident = do
    ref <- getChanMapRef <$> getYesod
    chan <- liftIO $ join $ maybe undefined return <$> M.lookup ident <$> readIORef ref

    codes <- T.lines <$> maybe undefined id <$> lookupPostParam "code"
    _ <- liftIO $ forkIO $ do
            forM_ codes $ \code -> do
                                   writeChan chan $ ServerEvent Nothing Nothing [fromText code]
                                   threadDelay (1000*1000)

    return ()
