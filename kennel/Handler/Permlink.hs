module Handler.Permlink (
  postPermlinkR,
  getLinkedPermlinkR
) where

import Import
import Handler.Root (makeRootR)
import qualified Data.Text as T
import System.Random (Random, randomRIO)
import Control.Monad (replicateM)
import Data.Char (chr)

randomRAny :: Random a => (a,a) -> (a -> Bool) -> IO a
randomRAny range p = do
  v <- randomRIO range
  if p v then return v
            else randomRAny range p

isLinkCode :: Int -> Bool
isLinkCode n | 48 <= n && n <= 57 = True -- 0-9
isLinkCode n | 65 <= n && n <= 90 = True -- A-Z
isLinkCode n | 97 <= n && n <= 122 = True -- a-z
isLinkCode _ = False

postPermlinkR :: Handler Value
postPermlinkR = do
    mCompiler <- lookupPostParam "compiler"
    mCode <- lookupPostParam "code"
    mOpts <- lookupPostParam "options"
    -- liftIO . (Just <$>) :: IO Code -> Handler (Maybe Code)
    mCodeInstance <- maybe (return Nothing) (liftIO . (Just <$>)) $
                         -- Maybe (IO Code)
                         makeCode <$> mCompiler
                                  <*> mCode
                                  <*> mOpts
    go mCodeInstance
  where
    go (Just code) = do
      link <- liftIO $ T.pack <$> (replicateM 16 $ chr <$> randomRAny (0,255) isLinkCode)
      _ <- runDB (insert $ Link link code)
      let json = object ["success" .= True, "link" .= link]
      returnJson json
    go _ = do
      let json = object ["success" .= False]
      returnJson json

getLinkedPermlinkR :: Text -> Handler Html
getLinkedPermlinkR link = do
    permlink <- runDB (getBy404 $ UniqueLink link)
    let code = linkCode $ entityVal permlink
    makeRootR (Just code)
