-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Import
import qualified Language.Haskell.TH                    as TH
import qualified Database.Persist.Sqlite                as PersistSqlite
import qualified Yesod.Default.Config                   as YDConfig
import qualified Yesod.Default.Util                     as YDUtil
import qualified Data.Text                              as T
import qualified Data.Yaml                              as Yaml
import qualified Data.Default                           as Default
import qualified Text.Hamlet                            as Hamlet

import Data.Yaml ((.:))
import Text.Shakespeare.Text (st)
import Settings.Development (development)

-- | Which Persistent backend this site is using.
type PersistConf = PersistSqlite.SqliteConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: YDConfig.AppConfig YDConfig.DefaultEnv x -> T.Text
staticRoot conf = [st|#{YDConfig.appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: YDUtil.WidgetFileSettings
widgetFileSettings = Default.def
    { YDUtil.wfsHamletSettings = Hamlet.defaultHamletSettings
        { Hamlet.hamletNewlines = Hamlet.AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> TH.Q TH.Exp
widgetFile = (if development then YDUtil.widgetFileReload
                             else YDUtil.widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCopyright :: T.Text
    , extraAuth :: Bool
    } deriving Show

parseExtra :: YDConfig.DefaultEnv -> Yaml.Object -> Yaml.Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:  "auth"
