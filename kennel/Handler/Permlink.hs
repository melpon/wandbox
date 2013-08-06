module Handler.Permlink (
  postPermlinkR,
  getLinkedPermlinkR
) where

import Import
import qualified Data.Text                              as T
import qualified System.Random                          as Random
import qualified Control.Monad                          as Monad
import qualified Yesod                                  as Y
import qualified Data.Char                              as Char

import Yesod ((.=))

import Foundation (Handler)
import Model (makeCode, Unique(UniqueLink), Link(..))
import Handler.Root (makeRootR)

randomRAny :: Random.Random a => (a,a) -> (a -> Bool) -> IO a
randomRAny range p = do
  v <- Random.randomRIO range
  if p v then return v
            else randomRAny range p

isLinkCode :: Int -> Bool
isLinkCode n | 48 <= n && n <= 57 = True -- 0-9
isLinkCode n | 65 <= n && n <= 90 = True -- A-Z
isLinkCode n | 97 <= n && n <= 122 = True -- a-z
isLinkCode _ = False

postPermlinkR :: Handler Y.Value
postPermlinkR = do
    mCompiler <- Y.lookupPostParam "compiler"
    mCode <- Y.lookupPostParam "code"
    mOpts <- Y.lookupPostParam "options"
    -- liftIO . (Just <$>) :: IO Code -> Handler (Maybe Code)
    mCodeInstance <- maybe (return Nothing) (Y.liftIO . (Just <$>)) $
                         -- Maybe (IO Code)
                         makeCode <$> mCompiler
                                  <*> mCode
                                  <*> mOpts
    go mCodeInstance
  where
    go (Just code) = do
      link <- Y.liftIO $ T.pack <$> (Monad.replicateM 16 $ Char.chr <$> randomRAny (0,255) isLinkCode)
      _ <- Y.runDB (Y.insert $ Link link code)
      let json = Y.object ["success" .= True, "link" .= link]
      Y.returnJson json
    go _ = do
      let json = Y.object ["success" .= False]
      Y.returnJson json

getLinkedPermlinkR :: T.Text -> Handler Y.Html
getLinkedPermlinkR link = do
    permlink <- Y.runDB (Y.getBy404 $ UniqueLink link)
    let code = linkCode $ Y.entityVal permlink
    makeRootR (Just code)
