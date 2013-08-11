import Import
import qualified Yesod.Default.Config                   as YDConfig
import qualified Yesod.Default.Main                     as YDMain
import qualified Options.Applicative                    as Options
import qualified System.Environment                     as Environment

import Options.Applicative ((<>))

import Settings             (parseExtra, Extra)
import Application          (makeApplication)

data Argument = Argument
  { argEnvironment :: YDConfig.DefaultEnv
  , argConfig :: FilePath
  }

argumentParser :: FilePath -> Options.Parser Argument
argumentParser configDef = Argument
  <$> Options.argument Options.auto
      (  Options.metavar "ENV"
      <> Options.completeWith envCompletes
      <> Options.help ("Environments: " ++ show envCompletes)
      )
  <*> Options.strOption
      (  Options.long "config"
      <> Options.short 'c'
      <> Options.metavar "CONFIG"
      <> Options.value configDef
      <> Options.showDefault
      <> Options.help "Path for a Config File"
      )
  where
    envCompletes = map show $ (enumFromTo minBound maxBound :: [YDConfig.DefaultEnv])

parseArgument :: IO Argument
parseArgument = do
    configDef <- maybe "config/settings.yml" id <$> lookup "CONFIG" <$> Environment.getEnvironment
    Options.execParser $ getInfo configDef
  where
    getInfo configDef = Options.info (Options.helper <*> (argumentParser configDef))
                ( Options.fullDesc
                )

fromArgs :: IO (YDConfig.AppConfig YDConfig.DefaultEnv Extra)
fromArgs = do
    argument <- parseArgument

    let env = argEnvironment argument
    let configPath = argConfig argument

    let cs = (YDConfig.configSettings env)
                { YDConfig.csParseExtra = parseExtra
                , YDConfig.csFile = \_ -> return configPath
                }
    config <- YDConfig.loadConfig cs

    return config

main :: IO ()
main = YDMain.defaultMain fromArgs makeApplication
