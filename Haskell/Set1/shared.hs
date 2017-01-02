module Shared
(byteStrToBase64,
 base64ToString,
 byteStrToBytes,
 bytesToStr
) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import qualified Data.Char as C
import qualified Data.ByteString.Base64 as B64
import Data.Hex


-- Converts a hex string representation of a byte into ByteString
byteStrToBytes :: String -> Maybe B.ByteString
byteStrToBytes str = case unhex str of Nothing -> Nothing
                                       Just s -> Just (C8.pack s)

bytesToStr :: B.ByteString -> String
bytesToStr = map (C.chr . fromEnum) . B.unpack

byteStrToBase64 :: String -> Maybe B.ByteString
byteStrToBase64 = encode' . byteStrToBytes
  where encode' Nothing = Nothing
        encode' (Just b) = Just (B64.encode b)

base64ToString :: B.ByteString -> String
base64ToString = bytesToStr . B64.decodeLenient

