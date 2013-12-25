module VM.Protocol (
  ProtocolSpecifier(..),
  Protocol(..),
  protocolParser,
  toString
) where

import Import
import qualified Control.Monad                          as Monad
import qualified Data.Attoparsec.ByteString             as AttoBS
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Char8                  as BSC
import qualified Data.Char                              as Char
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE
import qualified Data.Word                              as Word

import Control.Applicative (pure, (<*), (<|>))

import VM.QuotedPrintable (qpEncode, qpDecode)

data ProtocolSpecifier =
  Version |
  VersionResult |
  Control |
  Source |
  CompilerOption |
  CompilerMessageE |
  CompilerMessageS |
  CompilerOptionRaw |
  RuntimeOptionRaw |
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

toWord8 :: Char -> Word.Word8
toWord8 = fromIntegral . Char.ord

encode :: T.Text -> BS.ByteString
encode = qpEncode . TE.encodeUtf8
decode :: BS.ByteString -> Maybe T.Text
decode = (TE.decodeUtf8With onDecodeError <$>) . qpDecode
  where onDecodeError _ _ = Just '?'

specParser :: AttoBS.Parser ProtocolSpecifier
specParser = do
  r <- (pure VersionResult   <* AttoBS.try (AttoBS.string "VersionResult")) <|>
       (pure Version         <* AttoBS.try (AttoBS.string "Version")) <|>
       (pure Control         <* AttoBS.try (AttoBS.string "Control")) <|>
       (pure Source          <* AttoBS.try (AttoBS.string "Source")) <|>
       (pure CompilerOptionRaw <* AttoBS.try (AttoBS.string "CompilerOptionRaw")) <|>
       (pure CompilerOption  <* AttoBS.try (AttoBS.string "CompilerOption")) <|>
       (pure CompilerMessageE <* AttoBS.try (AttoBS.string "CompilerMessageE")) <|>
       (pure CompilerMessageS <* AttoBS.try (AttoBS.string "CompilerMessageS")) <|>
       (pure RuntimeOptionRaw <* AttoBS.try (AttoBS.string "RuntimeOptionRaw")) <|>
       (pure StdIn           <* AttoBS.try (AttoBS.string "StdIn")) <|>
       (pure StdOut          <* AttoBS.try (AttoBS.string "StdOut")) <|>
       (pure StdErr          <* AttoBS.try (AttoBS.string "StdErr")) <|>
       (pure ExitCode        <* AttoBS.try (AttoBS.string "ExitCode")) <|>
       (pure Signal          <* AttoBS.try (AttoBS.string "Signal"))
  _ <- AttoBS.word8 $ toWord8 ' '
  return r

protocolParser' :: AttoBS.Parser Protocol
protocolParser' = do
  spec <- specParser
  len <- read . BSC.unpack <$> AttoBS.takeWhile1 (AttoBS.inClass "0-9")
  _ <- AttoBS.word8 $ toWord8 ':'
  contents <- Monad.join $ maybe (fail "failed to decode contents") return <$> decode <$> AttoBS.take len
  _ <- AttoBS.word8 $ toWord8 '\n'
  return $ Protocol spec contents

protocolParser :: AttoBS.Parser Protocol
protocolParser = do
  proto <- (pure ProtocolNil <* AttoBS.word8 0) <|> protocolParser'
  return proto

toString :: Protocol -> BS.ByteString
toString ProtocolNil = BS.singleton 0
toString (Protocol spec contents) =
  let encstr = encode contents in
  BS.concat [BSC.pack $ show spec, " ", BSC.pack $ show $ BS.length encstr, ":", encstr, "\n"]

