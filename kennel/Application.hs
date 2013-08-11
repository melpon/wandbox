{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import

import qualified Yesod                                  as Y
import qualified Yesod.Default.Config                   as YDConfig
import qualified Yesod.Default.Main                     as YDMain
import qualified Network.Wai.Middleware.RequestLogger   as RequestLogger
import qualified Database.Persist                       as Persist
import qualified Database.Persist.Sql                   as PersistSql
import qualified Network.HTTP.Conduit                   as HConduit
import qualified Control.Monad.Logger                   as MonadLogger
import qualified System.IO                              as I
import qualified System.Log.FastLogger                  as FastLogger
import qualified Yesod.Auth.HashDB                      as YAuthHDB
import qualified System.Environment                     as Environment

import Yesod.Auth (getAuth)
import Yesod.Default.Handlers (getFaviconR, getRobotsR)

import ChanMap (newChanMap)
import Foundation (resourcesApp, App(..), getStatic, Route(..))
import Model (migrateAll)
import Settings (Extra, parseExtra, PersistConf)
import Settings.StaticFiles (staticSite)
import Settings.Development (development)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Root (getRootR)
import Handler.Compile (postCompileR, getEmptyCompileR)
import Handler.Source (getSourceR, getEmptySourceR)
import Handler.Permlink (postPermlinkR, getLinkedPermlinkR)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
Y.mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: YDConfig.AppConfig YDConfig.DefaultEnv Extra -> IO Y.Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- RequestLogger.mkRequestLogger HConduit.def
        { RequestLogger.outputFormat =
            if development
                then RequestLogger.Detailed True
                else RequestLogger.Apache RequestLogger.FromSocket
        , RequestLogger.destination = RequestLogger.Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- Y.toWaiAppPlain foundation
    return $ logWare app

migrates :: PersistSql.SqlPersistT (MonadLogger.LoggingT IO) ()
migrates = do
    PersistSql.runMigration YAuthHDB.migrateUsers
    PersistSql.runMigration migrateAll

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: YDConfig.AppConfig YDConfig.DefaultEnv Extra -> IO App
makeFoundation conf = do
    s <- staticSite
    dbconf <- YDConfig.withYamlEnvironment "config/sqlite.yml" (YDConfig.appEnv conf)
              Persist.loadConfig >>=
              Persist.applyEnv
    p <- Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- FastLogger.mkLogger True I.stdout
    cm <- Y.liftIO $ newChanMap
    let foundation = App conf s p dbconf logger cm

    -- Perform database migration using our application's logging settings.
    MonadLogger.runLoggingT
        (Persist.runPool dbconf migrates p)
        (Y.messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Y.Application)
getApplicationDev = do
    config <- maybe "config/settings.yml" id <$> lookup "CONFIG" <$> Environment.getEnvironment
    YDMain.defaultDevelApp (loader config) makeApplication
  where
    loader config = YDConfig.loadConfig (YDConfig.configSettings YDConfig.Development)
        { YDConfig.csParseExtra = parseExtra
        , YDConfig.csFile = \_ -> return config
        }
