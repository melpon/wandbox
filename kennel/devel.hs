{-# LANGUAGE PackageImports #-}
import Import
import qualified Network.Wai.Handler.Warp               as Warp
import qualified System.Directory                       as Directory
import qualified System.Exit                            as Exit
import qualified Control.Concurrent                     as Concurrent

import "kennel" Application (getApplicationDev)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    Concurrent.forkIO $ Warp.runSettings Warp.defaultSettings
        { Warp.settingsPort = port
        } app
    loop

loop :: IO ()
loop = do
  Concurrent.threadDelay 100000
  e <- Directory.doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = Exit.exitSuccess
