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
import Model (makeCode, Unique(UniqueLink), EntityField(LinkOutputLinkId, LinkOutputOrder), Link(..), LinkOutput(..))
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
    (Just compiler) <- Y.lookupPostParam "compiler"
    (Just code) <- Y.lookupPostParam "code"
    (Just opts) <- Y.lookupPostParam "options"
    codeInstance <- Y.liftIO $ makeCode compiler code opts
    linkName <- Y.liftIO $ T.pack <$> (Monad.replicateM 16 $ Char.chr <$> randomRAny (0,255) isLinkCode)
    typeOutputs <- Y.lookupPostParams "outputs[]"

    go codeInstance linkName typeOutputs
  where
    toOutput linkId (order, typeOutput) =
        let (typ, commaOutput) = T.break (','==) typeOutput
            output = T.drop 1 commaOutput
        in LinkOutput linkId order typ output
    go code linkName typeOutputs = do
      Y.runDB $ do
          codeId <- Y.insert code
          let link = Link linkName codeId
          linkId <- Y.insert link
          let outputs = map (toOutput linkId) $ zip [0..] typeOutputs
          _ <- Y.insertMany outputs
          return ()
      let json = Y.object ["success" .= True, "link" .= linkName]
      Y.returnJson json

getLinkedPermlinkR :: T.Text -> Handler Y.Html
getLinkedPermlinkR link = do
    permlink <- Y.runDB (Y.getBy404 $ UniqueLink link)
    let codeId = linkCodeId $ Y.entityVal permlink
    code <- Y.runDB (Y.get404 codeId)
    outputs <- Y.runDB (Y.selectList [LinkOutputLinkId Y.==. Y.entityKey permlink] [Y.Asc LinkOutputOrder])
    makeRootR (Just (code, map Y.entityVal outputs))
