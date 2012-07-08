module VM.Protocol (
  ProtocolSpecifier(..),
  Protocol(..),
  protocolParser,
  toString
) where

import Import
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.Monad (join)
import Control.Applicative ((<*), (<|>))
import Data.Char (ord)
import Data.Word (Word8)

import VM.QuotedPrintable (qpEncode, qpDecode)

data ProtocolSpecifier =
  Control |
  Source |
  CompilerOption |
  CompilerMessageE |
  CompilerMessageS |
  StdIn |
  StdOut |
  StdErr |
  ExitCode |
  Signal
  deriving (Show)

data Protocol = ProtocolNil | Protocol {
  protoSpec :: ProtocolSpecifier,
  protoContents :: T.Text
  } deriving (Show)

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

encode :: T.Text -> B.ByteString
encode = qpEncode . TE.encodeUtf8
decode :: B.ByteString -> Maybe T.Text
decode = (TE.decodeUtf8 <$>) . qpDecode

specParser :: AB.Parser ProtocolSpecifier
specParser = do
  r <- (pure Control         <* AB.try (AB.string "Control")) <|>
       (pure Source          <* AB.try (AB.string "Source")) <|>
       (pure CompilerOption  <* AB.try (AB.string "CompilerOption")) <|>
       (pure CompilerMessageE <* AB.try (AB.string "CompilerMessageE")) <|>
       (pure CompilerMessageS <* AB.try (AB.string "CompilerMessageS")) <|>
       (pure StdIn           <* AB.try (AB.string "StdIn")) <|>
       (pure StdOut          <* AB.try (AB.string "StdOut")) <|>
       (pure StdErr          <* AB.try (AB.string "StdErr")) <|>
       (pure ExitCode        <* AB.try (AB.string "ExitCode")) <|>
       (pure Signal          <* AB.try (AB.string "Signal"))
  _ <- AB.word8 $ toWord8 ' '
  return r

protocolParser' :: AB.Parser Protocol
protocolParser' = do
  spec <- specParser
  len <- read . BC.unpack <$> AB.takeWhile1 (AB.inClass "0-9")
  _ <- AB.word8 $ toWord8 ':'
  contents <- join $ maybe (fail "failed to decode contents") return <$> decode <$> AB.take len
  _ <- AB.word8 $ toWord8 '\n'
  return $ Protocol spec contents

protocolParser :: AB.Parser Protocol
protocolParser = do
  proto <- (pure ProtocolNil <* AB.word8 0) <|> protocolParser'
  return proto

toString :: Protocol -> B.ByteString
toString ProtocolNil = B.singleton 0
toString (Protocol spec contents) =
  let encstr = encode contents in
  B.concat [BC.pack $ show spec, " ", BC.pack $ show $ B.length encstr, ":", encstr, "\n"]

