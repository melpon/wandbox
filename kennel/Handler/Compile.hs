module Handler.Compile (
  postCompileR
, getEmptyCompileR
) where

import Import
import qualified Control.Concurrent                     as Concurrent
import qualified Data.Text                              as T
import qualified Yesod                                  as Y

import Foundation (Handler, getChanMap, getExtra)
import Settings (Extra(..))
import Model (makeCode)
import ChanMap (writeChan)
import Api (runCode, sinkEventSource)

getEmptyCompileR :: Handler ()
getEmptyCompileR = Y.notFound

postCompileR :: T.Text -> Handler ()
postCompileR ident = do
  (Just compiler) <- Y.lookupPostParam "compiler"
  (Just code) <- Y.lookupPostParam "code"
  (Just options) <- Y.lookupPostParam "options"
  codeInstance <- Y.liftIO $ makeCode compiler code options
  host <- extraVMHost <$> getExtra
  port <- extraVMPort <$> getExtra
  _ <- go host port codeInstance
  return ()
  where
    go host port codeInstance = do
      cm <- getChanMap <$> Y.getYesod
      _ <- Y.liftIO $ Concurrent.forkIO $ runCode host port codeInstance $ sinkEventSource $ writeChan cm ident
      return ()
