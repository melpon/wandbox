module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi

import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Applicative ((<$>))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

makeCode :: Text -> Text -> Text -> IO Code
makeCode compiler code options =
    Code compiler code False False options <$> getCurrentTime
