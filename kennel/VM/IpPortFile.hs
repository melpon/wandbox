module VM.IpPortFile (
  ipPortFile
) where

import Import
import System.IO (readFile)
import Language.Haskell.TH
import Network (PortID(PortNumber))

ipPortFile :: FilePath -> Q Exp              
ipPortFile fp = do                           
  [ip, port] <- runIO $ lines <$> readFile fp
  let port' = read port :: Int
  [| (ip, PortNumber $([|port'|])) |]
