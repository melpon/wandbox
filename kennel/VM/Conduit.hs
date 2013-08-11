{-# LANGUAGE TemplateHaskell #-}
module VM.Conduit (
  connectVM,
  sendVM,
  receiveVM
) where

import Import
import qualified Data.Conduit                           as Conduit
import qualified Data.Conduit.Binary                    as ConduitB
import qualified Data.Conduit.List                      as ConduitL
import qualified Data.Attoparsec.ByteString             as AttoBS
import qualified Data.ByteString                        as BS
import qualified System.IO                              as I
import qualified Network                                as N

import Data.Conduit ((=$), ($=))

import VM.IpPortFile (ipPortFile)
import VM.Protocol (Protocol(..), protocolParser, toString)

parsePipe :: Monad m => Conduit.Conduit BS.ByteString m (Either String Protocol)
parsePipe = go protoParse
  where
    go parse = do
      mInput <- Conduit.await
      case mInput of
        Nothing -> return ()
        (Just input) -> do
          let (parse',results) = parseWhile parse input
          mapM_ Conduit.yield results
          go parse'
    protoParse = AttoBS.parse protocolParser
    parseWhile parse input = do
      case parse input of
        (AttoBS.Done remain result) -> let (parse',xs) = parseWhile protoParse remain
                                   in (parse',Right result:xs)
        (AttoBS.Partial f)          -> (f,[])
        (AttoBS.Fail a b c)         -> (protoParse,[Left $ show (a,b,c)])

connectVM :: IO I.Handle
connectVM = uncurry N.connectTo $(ipPortFile "config/vmip")

sendVM :: Conduit.MonadResource m => I.Handle -> Conduit.Sink Protocol m ()
sendVM handle = ConduitL.map toString =$ ConduitB.sinkHandle handle

receiveVM :: Conduit.MonadResource m => I.Handle -> Conduit.Source m (Either String Protocol)
receiveVM handle = ConduitB.sourceHandle handle $= parsePipe
