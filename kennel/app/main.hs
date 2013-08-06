import Import
import qualified Yesod.Default.Config                   as YDConfig
import qualified Yesod.Default.Main                     as YDMain

import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main = YDMain.defaultMain (YDConfig.fromArgs parseExtra) makeApplication
