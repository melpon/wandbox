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
import qualified Codec.Binary.QuotedPrintable as QP
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.Monad (join)
import Control.Applicative ((<*), (<|>))
import Data.Char (ord)
import Data.Word (Word8)

data ProtocolSpecifier =
  Control |
  Source |
  CompilerSwitch |
  CompilerMessage |
  StdIn |
  StdOut |
  StdErr |
  ExitCode |
  Signal
  deriving (Show)

data Protocol = Protocol {
  protoSpec :: ProtocolSpecifier,
  protoContents :: T.Text
  } deriving (Show)

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

encode :: T.Text -> B.ByteString
encode = BC.pack . QP.encode . B.unpack . TE.encodeUtf8
decode :: B.ByteString -> Maybe T.Text
decode = (TE.decodeUtf8 . B.pack <$>) . QP.decode . BC.unpack

specParser :: AB.Parser ProtocolSpecifier
specParser = do
  r <- (pure Control         <* AB.try (AB.string "Control")) <|>
       (pure Source          <* AB.try (AB.string "Source")) <|>
       (pure CompilerSwitch  <* AB.try (AB.string "CompilerSwitch")) <|>
       (pure CompilerMessage <* AB.try (AB.string "CompilerMessage")) <|>
       (pure StdIn           <* AB.try (AB.string "StdIn")) <|>
       (pure StdOut          <* AB.try (AB.string "StdOut")) <|>
       (pure StdErr          <* AB.try (AB.string "StdErr")) <|>
       (pure ExitCode        <* AB.try (AB.string "ExitCode")) <|>
       (pure Signal          <* AB.try (AB.string "Signal"))
  _ <- AB.word8 $ toWord8 ' '
  return r

protocolParser :: AB.Parser Protocol
protocolParser = do
  spec <- specParser
  len <- read . BC.unpack <$> AB.takeWhile1 (AB.inClass "0-9")
  _ <- AB.word8 $ toWord8 ':'
  contents <- join $ maybe (fail "failed to decode contents") return <$> decode <$> AB.take len
  return $ Protocol spec contents

toString :: Protocol -> B.ByteString
toString (Protocol spec contents) =
  let encstr = encode contents in
  B.concat [BC.pack $ show spec, " ", BC.pack $ show $ B.length encstr, ":", encstr]

