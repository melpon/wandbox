{-# LANGUAGE OverloadedStrings #-}
module VM.QuotedPrintable (
  qpEncode,
  qpDecode
) where

import Import

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC ()

import Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))

toHex :: Word8 -> (Word8, Word8)
toHex w = (BS.index tbl hn, BS.index tbl ln)
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

qpEncode :: BS.ByteString -> BS.ByteString
qpEncode = BS.pack . reverse . fst . BS.foldl' f ([],0)
  where
    isEnc w = w < 33 || w == 61 || w > 126
    f :: ([Word8],Int) -> Word8 -> ([Word8],Int)
    f (rx,n) w | isEnc w && n >= 73 = let (a,b) = toHex w in (b:a:61:10:61:rx,3)
               | isEnc w            = let (a,b) = toHex w in (b:a:61:rx,n+3)
               |            n >= 75 = (w:10:61:rx,1)
               | otherwise          = (w:rx,n+1)

qpDecode :: BS.ByteString -> Maybe BS.ByteString
qpDecode = (BS.pack <$>) . decodeR . BS.unpack
  where
    decodeR []          = Just []
    decodeR (61:10:ys)  = decodeR ys
    decodeR (61:a:b:ys) = do c <- fromHex a b
                             (c:) <$> decodeR ys
    decodeR (61:_)      = Nothing
    decodeR (x:ys)      = (x:) <$> decodeR ys
