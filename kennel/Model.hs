module Model
  ( Y.Unique(..)
  , Y.EntityField(..)
  , Code(..)
  , CodeId
  , Link(..)
  , LinkId
  , LinkOutput(..)
  , LinkOutputId
  , migrateAll
  , makeCode
  ) where

import Import
import qualified Yesod                                  as Y
import qualified Data.Text                              as T
import qualified Database.Persist.Quasi                 as PersistQuasi
import qualified Data.Time.Clock                        as Clock

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
Y.share [Y.mkPersist Y.sqlOnlySettings, Y.mkMigrate "migrateAll"]
    $(Y.persistFileWith PersistQuasi.lowerCaseSettings "config/models")

makeCode :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> IO Code
makeCode compiler code options compilerOptionRaw runtimeOptionRaw stdin =
    Code compiler code False False options compilerOptionRaw runtimeOptionRaw stdin <$> Clock.getCurrentTime
