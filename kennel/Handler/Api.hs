module Handler.Api (
  getApiListR
, postApiCompileR
) where

import Import

import qualified Yesod                                  as Y
import qualified Data.HashMap.Strict                    as HMS
import qualified Data.Aeson                             as Aeson

import Settings (Extra(..))
import Foundation (Handler, getExtra)
import Model (makeCode)
import Api (getCompilerInfos, runCode, sinkJson)

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
    let (Just (Y.String options)) = HMS.lookup "options" obj
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
