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
import Network (connectTo, PortID(PortNumber))

import VM.IpPortFile (ipPortFile)
import VM.Protocol (Protocol(..), protocolParser, toString)

parsePipe :: Monad m => C.Conduit B.ByteString m (Either String Protocol)
parsePipe = CL.map (AB.parseOnly protocolParser)

connectVM :: IO Handle
connectVM = uncurry connectTo $(ipPortFile "config/vmip")

sendVM :: C.ResourceIO m => Handle -> C.Sink Protocol m ()
sendVM handle = CL.map toString =$ CB.sinkHandle handle

receiveVM :: C.ResourceIO m => Handle -> C.Source m (Either String Protocol)
receiveVM handle = CB.sourceHandle handle $= CB.lines $= parsePipe
