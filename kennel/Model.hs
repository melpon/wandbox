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
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

makeCode :: Text -> Text -> Bool -> Bool -> IO Code
makeCode compiler code optimize warning =
    Code compiler code optimize warning <$> getCurrentTime
