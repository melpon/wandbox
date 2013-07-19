{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings (parseExtra, PersistConfig)
import Settings.StaticFiles (staticSite)
import Yesod.Auth
import Yesod.Default.Config (AppConfig(..), ConfigSettings(..), withYamlEnvironment, configSettings, loadConfig)
import Yesod.Default.Main (defaultDevelApp)
import Yesod.Default.Handlers (getFaviconR, getRobotsR)
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import Network.Wai (Application)
import ChanMap (newChanMap)
import Model
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration, SqlPersist)
import Yesod.Auth.HashDB (migrateUsers)
import qualified Settings.CompilerConfig as CC

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Compile
import Handler.Source
import Handler.Permlink

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

migrates :: SqlPersist IO ()
migrates = do
    runMigration migrateUsers
    runMigration migrateAll

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig AppEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    s <- staticSite conf
    let sqliteSetting = extraSqliteSetting $ appExtra conf

    dbconf <- withYamlEnvironment sqliteSetting (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf migrates p

    cm <- liftIO $ newChanMap
    cc <- liftIO $ CC.loadConfig $ extraCompilerConfig $ appExtra conf
    let foundation = App conf setLogger s cm p dbconf cc
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

