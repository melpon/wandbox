module Settings.CompilerConfig
  ( loadConfig
  , Config
  , tojson
  ) where

import Prelude
import Data.Functor ((<$>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

data Config = Config A.Value

instance A.FromJSON Config where
  parseJSON v = return $ Config v

instance A.ToJSON Config where
  toJSON (Config v) = v

loadConfig :: FilePath -> IO Config
loadConfig path = do
  text <- BL.readFile path
  let (Just result) = A.decode text
  return result

tojson :: Config -> TL.Text
tojson config = TLE.decodeUtf8 $ A.encode config
