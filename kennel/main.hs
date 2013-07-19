import Prelude              (IO)
import Yesod.Default.Main   (defaultMain)
import Yesod.Default.Config (fromArgs)
import Settings             (parseExtra)
import Application          (getApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) getApplication
