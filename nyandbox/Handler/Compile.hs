module Handler.Compile (
  getSourceR,
  postCompileR
) where

import Import
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import qualified Data.Text as T
import qualified ChanMap as CM
import Control.Exception (bracket)
import System.IO (hClose)

import qualified Data.Conduit as C
import Data.Conduit ((=$), ($=), ($$))
import qualified Data.Conduit.List as CL

import VM.Protocol (Protocol(..), ProtocolSpecifier(..))
import VM.Conduit (connectVM, sendVM, receiveVM)

getSourceR :: Text -> Handler ()
getSourceR ident = do
    cm <- getChanMap <$> getYesod
    chan <- liftIO $ CM.insertLookup cm ident

    req <- waiRequest
    res <- lift $ eventSourceAppChan chan req

    sendWaiResponse res

vmHandle :: IO ()
vmHandle =
  bracket connectVM hClose $ \handle -> do
    C.runResourceT $ CL.sourceList [Protocol Control "compiler=ghc"] $$ sendVM handle
    C.runResourceT $ receiveVM handle $$ CL.mapM_ print

postCompileR :: Text -> Handler ()
postCompileR ident = do
  mCode <- lookupPostParam "code"
  maybe (return ()) go mCode
  where
    go code = do
      cm <- getChanMap <$> getYesod
      let codes = T.lines code
      _ <- liftIO $ forkIO $ do
              forM_ codes $ \line -> do
                                     CM.writeChan cm ident $ ServerEvent Nothing Nothing [fromText line]
                                     threadDelay (1000*1000)
      return ()
