module Handler.Compile where

import Import
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM, forM_)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import System.Random (randomRIO)
import qualified Data.Text as T
import qualified ChanMap as CM
import Control.Exception (bracket)
import System.IO (hClose)

import qualified Data.Conduit as C
import Data.Conduit ((=$), ($=), ($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

getSourceR :: Handler ()
getSourceR = do
    ident <- liftIO $ T.pack <$> (replicateM 16 $ randomRIO ('a','z'))
    cm <- getChanMap <$> getYesod
    chan <- liftIO $ CM.insert cm ident

    req <- waiRequest
    res <- lift $ eventSourceAppChan chan req

    _ <- liftIO $ CM.writeChan cm ident $ ServerEvent Nothing Nothing [fromText ident]

    sendWaiResponse res

vmHandle :: IO ()
vmHandle =
  bracket connectVM hClose $ \handle -> do
    C.runResourceT $ CL.sourceList [Protocol Control "compiler=ghc"] $$ sendVM handle
    C.runResourceT $ receiveVM handle $$ CL.mapM_ print

postCompileR :: Text -> Handler ()
postCompileR ident = do
    cm <- getChanMap <$> getYesod

    codes <- T.lines <$> maybe undefined id <$> lookupPostParam "code"
    _ <- liftIO $ forkIO $ do
            forM_ codes $ \code -> do
                                   CM.writeChan cm ident $ ServerEvent Nothing Nothing [fromText code]
                                   threadDelay (1000*1000)
            --CM.writeChan cm ident CloseEvent
            CM.delete cm ident

    return ()
