{-# LANGUAGE OverloadedStrings #-}
module VM.QuotedPrintable (
  qpEncode,
  qpDecode
) where

import Import
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC ()
import Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))

toHex :: Word8 -> B.ByteString
toHex w = B.pack [B.index tbl hn, B.index tbl ln]
  where
    tbl = "0123456789ABCDEF"
    hn = fromIntegral $ w `shiftR` 4
    ln = fromIntegral $ w .&. 0xf

fromHex :: Word8 -> Word8 -> Maybe Word8
fromHex a b = dec <$> tbl a <*> tbl b
  where
    tbl w | 48 <= w && w <=  57 = Just $ w - 48 -- 0-9
          | 65 <= w && w <=  70 = Just $ w - 55 -- A-F
          | 97 <= w && w <= 102 = Just $ w - 87 -- a-f
          | otherwise           = Nothing
    dec hn ln = hn `shiftL` 4 .|. ln

qpEncode :: B.ByteString -> B.ByteString
qpEncode = softLineBreak . B.concatMap f
  where
    f w | w < 33 || w == 61 || w > 126 = B.cons 61 $ toHex w
        | otherwise                    = B.singleton w
    softLineBreak = B.concat . split
      where split xs | B.length xs > 76 = let (a, b) = B.splitAt 75 xs
                                          in a : "\n" : split b
                     | otherwise        = [xs]

qpDecode :: B.ByteString -> Maybe B.ByteString
qpDecode = (B.pack . reverse <$>) . decodeR [] . B.unpack . concatLineBreak
  where
    concatLineBreak = B.concat . tokenize
      where tokenize bs = let (a,b) = B.breakSubstring "=\n" bs
                          in a : if B.null b then [] else tokenize (B.drop 2 b)
    decodeR xs []          = Just xs
    decodeR xs (61:a:b:ys) = do c <- fromHex a b
                                decodeR (c:xs) ys
    decodeR _  (61:_)      = Nothing
    decodeR xs (y:ys)      = decodeR (y:xs) ys
