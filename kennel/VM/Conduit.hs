{-# LANGUAGE TemplateHaskell #-}
module VM.Conduit (
  connectVM,
  sendVM,
  receiveVM
) where

import Import
import qualified Data.Conduit as C
import Data.Conduit ((=$), ($=))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import System.IO (Handle)
import Network (connectTo)

import VM.IpPortFile (ipPortFile)
import VM.Protocol (Protocol(..), protocolParser, toString)

type ProtoParse = B.ByteString -> AB.Result Protocol

parsePipe :: Monad m => C.Conduit B.ByteString m (Either String Protocol)
parsePipe = C.conduitState protoParse push close
  where
    protoParse = AB.parse protocolParser
    parseWhile parse input =
      case parse input of
        (AB.Done remain result) -> let (parse',xs) = parseWhile protoParse remain
                                   in (parse',Right result:xs)
        (AB.Partial f)          -> (f,[])
        (AB.Fail a b c)         -> (protoParse,[Left $ show (a,b,c)])
    push parse input = let (parse',results) = parseWhile parse input
                       in return $ C.StateProducing parse' results
    close _ = return []

connectVM :: IO Handle
connectVM = uncurry connectTo $(ipPortFile "config/vmip")

sendVM :: C.MonadResource m => Handle -> C.Sink Protocol m ()
sendVM handle = CL.map toString =$ CB.sinkHandle handle

receiveVM :: C.MonadResource m => Handle -> C.Source m (Either String Protocol)
receiveVM handle = CB.sourceHandle handle $= parsePipe
