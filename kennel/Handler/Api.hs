module Handler.Api (
  getApiListR
, postApiCompileR
, getApiPermlinkR
) where

import Import

import qualified Yesod                                  as Y
import qualified Data.HashMap.Strict                    as HMS
import qualified Data.Aeson                             as Aeson
import qualified Data.Text                              as T

import Settings (Extra(..))
import Foundation (Handler, getExtra)
import Model
    ( makeCode
    , Unique(UniqueLink)
    , EntityField
        ( LinkOutputLinkId
        , LinkOutputOrder
        )
    )
import Api (getCompilerInfos, runCode, sinkJson, getPermlink)

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
    codeInstance <- Y.liftIO $ makeCode compiler code options compilerOptionRaw runtimeOptionRaw stdin
    host <- extraVMHost <$> getExtra
    port <- extraVMPort <$> getExtra
    json <- Y.liftIO $ runCode host port codeInstance $ sinkJson
    Y.returnJson json
  where
    fromForm = do
      (pp, _) <- Y.runRequestBody
      let (Y.Object obj) = Y.object $ map (uncurry (Y..=)) pp
      return obj
    fromJson = Y.parseJsonBody_ :: Handler Aeson.Object

getApiPermlinkR :: T.Text -> Handler Y.Value
getApiPermlinkR link = do
    permlink <- Y.runDB (Y.getBy404 $ UniqueLink link)
    outputs <- Y.runDB (Y.selectList [LinkOutputLinkId Y.==. Y.entityKey permlink] [Y.Asc LinkOutputOrder])
    json <- getPermlink $ map Y.entityVal outputs
    Y.returnJson json
