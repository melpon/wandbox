module Settings.StaticFiles where

import Prelude
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir,AppEnv,Extra(..))
import Yesod.Default.Config (AppConfig(..))

-- | use this to create your static file serving site
staticSite :: AppConfig AppEnv Extra -> IO Static.Static
staticSite conf =
#ifdef DEVELOPMENT
    Static.staticDevel dir
#else
    Static.static dir
#endif
  where
    dir = Settings.staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)
