module ApiTypes (
  CompilerSwitchSelectOption(..)
, CompilerSwitch(..)
, CompilerVersion(..)
, CompilerInfo(..)
) where

import Import

import qualified Control.Monad                          as Monad
import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Types                       as AesonTypes
import qualified Data.Text                              as T

import Data.Aeson ((.:), (.=))

data CompilerSwitchSelectOption = CompilerSwitchSelectOption
  { swmoName :: T.Text
  , swmoDisplayName :: T.Text
  , swmoDisplayFlags :: T.Text
  } deriving (Show)
instance Aeson.FromJSON CompilerSwitchSelectOption where
  parseJSON (Aeson.Object v) =
    CompilerSwitchSelectOption <$>
      v .: "name" <*>
      v .: "display-name" <*>
      v .: "display-flags"
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerSwitchSelectOption where
  toJSON (CompilerSwitchSelectOption name displayName displayFlags) =
    Aeson.object
      [ "name" .= name
      , "display-name" .= displayName
      , "display-flags" .= displayFlags
      ]

data CompilerSwitch =
  CompilerSwitchSingle
  { swsName :: T.Text
  , swsFlags :: T.Text
  , swsDefault :: Bool
  , swsDisplayName :: T.Text
  } |
  CompilerSwitchSelect
  { swmDefault :: T.Text
  , swmOptions :: [CompilerSwitchSelectOption]
  }
  deriving (Show)

instance Aeson.FromJSON CompilerSwitch where
  parseJSON (Aeson.Object v) = do
    typ <- v .: "type" :: AesonTypes.Parser String
    if typ == "single"
      then
        CompilerSwitchSingle <$>
          v .: "name" <*>
          v .: "display-flags" <*>
          v .: "default" <*>
          v .: "display-name"
      else
        CompilerSwitchSelect <$>
          v .: "default" <*>
          v .: "options"
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerSwitch where
  toJSON (CompilerSwitchSingle name flags def displayName) =
    Aeson.object
      [ "name" .= name
      , "display-flags" .= flags
      , "default" .= def
      , "display-name" .= displayName
      ]
  toJSON (CompilerSwitchSelect def options) =
    Aeson.object
      [ "default" .= def
      , "options" .= options
      ]

data CompilerVersion = CompilerVersion
  { verName :: T.Text
  , verLanguage :: T.Text
  , verDisplayName :: T.Text
  , verVersion :: T.Text
  , verCompileCommand :: T.Text
  , verCompilerOptionRaw :: Bool
  , verRuntimeOptionRaw :: Bool
  } deriving (Show)
instance Aeson.FromJSON CompilerVersion where
  parseJSON (Aeson.Object v) =
    CompilerVersion <$>
      v .: "name" <*>
      v .: "language" <*>
      v .: "display-name" <*>
      v .: "version" <*>
      v .: "display-compile-command" <*>
      v .: "compiler-option-raw" <*>
      v .: "runtime-option-raw"
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerVersion where
  toJSON (CompilerVersion name language displayName version compileCommand compilerOptionRaw runtimeOptionRaw) =
    Aeson.object
      [ "name" .= name
      , "language" .= language
      , "display-name" .= displayName
      , "version" .= version
      , "display-compile-command" .= compileCommand
      , "compiler-option-raw" .= compilerOptionRaw
      , "runtime-option-raw" .= runtimeOptionRaw
      ]

data CompilerInfo = CompilerInfo
  { ciVersion :: CompilerVersion
  , ciSwitches :: [CompilerSwitch]
  } deriving (Show)

instance Aeson.FromJSON CompilerInfo where
  parseJSON json@(Aeson.Object v) = do
    version <- Aeson.parseJSON json :: AesonTypes.Parser CompilerVersion
    switches <- Monad.join (Aeson.parseJSON <$> (v .: "switches")) :: AesonTypes.Parser [CompilerSwitch]
    return $ CompilerInfo version switches
  parseJSON _ = Monad.mzero
instance Aeson.ToJSON CompilerInfo where
  toJSON (CompilerInfo version switch) =
    Aeson.object
      [ "name" .= verName version
      , "language" .= verLanguage version
      , "display-name" .= verDisplayName version
      , "version" .= verVersion version
      , "display-compile-command" .= verCompileCommand version
      , "compiler-option-raw" .= verCompilerOptionRaw version
      , "runtime-option-raw" .= verRuntimeOptionRaw version
      , "switches" .= switch
      ]

