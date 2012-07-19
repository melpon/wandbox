import Prelude              (IO)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra, loadConfigFromArgs)
import Application          (getApplication)

main :: IO ()
main = defaultMain (loadConfigFromArgs parseExtra) getApplication
