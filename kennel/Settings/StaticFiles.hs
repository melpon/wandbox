module Settings.StaticFiles where

import Import
import qualified Yesod.Static                           as YStatic
import qualified Data.Default                           as Default
import qualified Language.Haskell.TH                    as TH

import Yesod.Static (StaticRoute, Route(..))

import Settings (staticDir)
import Settings.Development (development)

-- | use this to create your static file serving site
staticSite :: IO YStatic.Static
staticSite = if development then YStatic.staticDevel staticDir
                            else YStatic.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(YStatic.staticFiles staticDir)

combineSettings :: YStatic.CombineSettings
combineSettings = Default.def

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: TH.Name -> [YStatic.Route YStatic.Static] -> TH.Q TH.Exp
combineStylesheets = YStatic.combineStylesheets' development combineSettings

combineScripts :: TH.Name -> [Route YStatic.Static] -> TH.Q TH.Exp
combineScripts = YStatic.combineScripts' development combineSettings
