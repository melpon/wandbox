module Handler.Api (
  getApiListR
, postApiCompileR
, getApiPermlinkR
) where

import Import

import qualified Yesod                                  as Y
import qualified Data.HashMap.Strict                    as HMS
import qualified Data.Aeson                             as Aeson
import qualified Data.Conduit.List                      as ConduitL
import qualified Data.Either                            as Either
import qualified Data.Text                              as T

import Data.Conduit (($$))
import Settings (Extra(..))
import Foundation (Handler, getExtra, Route(LinkedPermlinkR))
import Model
    ( makeCode
    , Unique(UniqueLink)
    , EntityField
        ( LinkOutputLinkId
        , LinkOutputOrder
        )
    , Link(..)
    )
import Api
    ( getCompilerInfos
    , runCode
    , sinkProtocols
    , sinkJson
    , saveForPermlink
    , getPermlink
    )

getApiListR :: Handler Y.Value
getApiListR = do
    host <- extraVMHost <$> getExtra
    port <- extraVMPort <$> getExtra
    compilerInfos <- Y.liftIO (getCompilerInfos host port)
    Y.returnJson compilerInfos

postApiCompileR :: Handler Y.Value
postApiCompileR = do
    (Just contentType) <- Y.lookupHeader "Content-Type"
    obj <- case contentType of
             "application/x-www-form-urlencoded" -> fromForm
             "application/json" -> fromJson
             _ -> fromJson

    Y.liftIO $ print obj
    let (Just (Y.String compiler)) = HMS.lookup "compiler" obj
    let (Just (Y.String code)) = HMS.lookup "code" obj
    let (Y.String options) = maybe "" id $ HMS.lookup "options" obj
    let (Y.String compilerOptionRaw) = maybe "" id $ HMS.lookup "compiler-option-raw" obj
    let (Y.String runtimeOptionRaw) = maybe "" id $ HMS.lookup "runtime-option-raw" obj
    let (Y.String stdin) = maybe "" id $ HMS.lookup "stdin" obj
    let (Y.Bool save) = maybe (Y.Bool False) id $ HMS.lookup "save" obj
    codeInstance <- Y.liftIO $ makeCode compiler code options compilerOptionRaw runtimeOptionRaw stdin
    host <- extraVMHost <$> getExtra
    port <- extraVMPort <$> getExtra
    protocols <- Y.liftIO $ runCode host port codeInstance $ sinkProtocols
    let validProtocols = Either.rights protocols
    json <- ConduitL.sourceList protocols $$ sinkJson
    jsonWithPermlink <-
      if save
        then do
          render <- Y.getUrlRender
          permlink <- saveForPermlink codeInstance validProtocols
          return $ insertPermlink render permlink json
        else do
          return json
    Y.returnJson jsonWithPermlink
  where
    fromForm = do
      (pp, _) <- Y.runRequestBody
      let (Y.Object obj) = Y.object $ map (uncurry (Y..=)) pp
      return obj
    fromJson = Y.parseJsonBody_ :: Handler Aeson.Object
    insertPermlink render permlink (Aeson.Object obj) =
      let
        obj' = HMS.insert "permlink" (Aeson.String permlink) obj
        obj'' = HMS.insert "url" (toUrl render permlink) obj'
      in
        Aeson.Object obj''
    insertPermlink _ _ _ = undefined
    toUrl render permlink = Aeson.String $ render $ LinkedPermlinkR permlink

getApiPermlinkR :: T.Text -> Handler Y.Value
getApiPermlinkR link = do
    permlink <- Y.runDB (Y.getBy404 $ UniqueLink link)
    let codeId = linkCodeId $ Y.entityVal permlink
    code <- Y.runDB (Y.get404 codeId)
    outputs <- Y.runDB (Y.selectList [LinkOutputLinkId Y.==. Y.entityKey permlink] [Y.Asc LinkOutputOrder])
    json <- getPermlink code (map Y.entityVal outputs)
    Y.returnJson json
