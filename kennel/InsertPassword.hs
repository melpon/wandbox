{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod.Auth.HashDB
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Data.Text (pack)

-- ghc -hide-package text-0.11.2.1 -package-conf cabal-dev/packages-7.4.1.conf InsertPassword.hs
main :: IO ()
main = do
  args <- getArgs
  if length args < 3 then
    putStrLn "InsertPassword <db> <userid> <passwd>"
  else do
    let [db, userid, passwd] = map pack args
    withSqliteConn db $ runSqlConn $ do
      runMigration migrateUsers
      let user = User userid passwd ""
      salted <- setPassword passwd user
      insert salted
      return ()
