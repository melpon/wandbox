module VM.IpPortFile (
  ipPortFile
) where

import Import
import qualified Language.Haskell.TH                    as TH
import qualified System.IO                              as I
import qualified Network                                as N

ipPortFile :: FilePath -> TH.Q TH.Exp              
ipPortFile fp = do                           
  [ip, port] <- TH.runIO $ lines <$> I.readFile fp
  let port' = read port :: Int
  [| (ip, N.PortNumber $([|port'|])) |]
