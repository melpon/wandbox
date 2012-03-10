{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network as N
import System.IO (hGetLine, hPutStrLn, hFlush)
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Char8 as BC

import qualified Protocol as P

sendResult h = do
  sendWithWait $ P.Protocol P.StdOut "stdout"
  sendWithWait $ P.Protocol P.StdErr "stderr"
  sendWithWait $ P.Protocol P.Signal "signal"
  sendWithWait $ P.Protocol P.ExitCode "0"
  sendWithWait $ P.ProtocolNil
  where
    sendWithWait proto = do
      hPutStrLn h $ BC.unpack $ P.toString proto
      hFlush h
      threadDelay (1*1000*1000)

receive h = do
  line <- hGetLine h
  print line
  let eProto = AB.parseOnly P.protocolParser $ BC.pack line
  matchProto eProto
  where
    matchProto (Left str) = putStrLn str
    matchProto (Right P.ProtocolNil) = print P.ProtocolNil
    matchProto (Right (P.Protocol P.Control text)) | text == "run" = sendResult h
    matchProto (Right (P.Protocol _ _)) = receive h

main :: IO ()
main = N.withSocketsDo $ do
  putStrLn "start listen"
  forever $
    bracket (N.listenOn (N.PortNumber 3001)) N.sClose $ \socket -> do
      (handle, _, _) <- N.accept socket
      forkIO $ receive handle
