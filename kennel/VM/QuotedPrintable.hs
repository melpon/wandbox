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
fromHex a b = dec (tbl a) (tbl b)
  where
    tbl w | 48 <= w && w <=  57 = Just $ w - 48
          | 65 <= w && w <=  70 = Just $ w - 55
          | 97 <= w && w <= 102 = Just $ w - 87
          | otherwise           = Nothing
    dec (Just hn) (Just ln) = Just $ hn `shiftL` 4 .|. ln
    dec _         _         = Nothing

qpEncode :: B.ByteString -> B.ByteString
qpEncode = softLineBreak B.empty . B.concatMap f
  where
    f w | w < 33 || w == 61 || w > 126 = B.cons 61 $ toHex w
        | otherwise                    = B.singleton w
    softLineBreak xs ys | B.length ys > 76 = let (a, b) = B.splitAt 75 ys in softLineBreak (B.concat [xs, a, "=\n"]) b
                        | otherwise        = B.concat [xs, ys]

qpDecode :: B.ByteString -> Maybe B.ByteString
qpDecode = (B.pack . reverse <$>) . decodeR [] . B.unpack . concatLineBreak
  where
    concatLineBreak = B.concat . tokenize
    tokenize bs = h : if B.null t then [] else tokenize (B.drop 2 t)
      where (h,t) = B.breakSubstring "=\n" bs
    decodeR xs []          = Just xs
    decodeR xs (61:a:b:ys) = do c <- fromHex a b
                                decodeR (c:xs) ys
    decodeR _  (61:_)      = Nothing
    decodeR xs (y:ys)      = decodeR (y:xs) ys
