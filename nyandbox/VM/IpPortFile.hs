module VM.IpPortFile (
  ipPortFile
) where

import Import
import System.IO (readFile)
import Language.Haskell.TH

ipPortFile :: FilePath -> Q Exp
ipPortFile fp = do [ip, port] <- lines <$> runIO (readFile fp)
                   return $ TupE [LitE $ StringL ip, AppE (ConE $ mkName "PortNumber") (LitE $ IntegerL $ read port)]

