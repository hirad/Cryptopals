import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import qualified Data.Char as C
import qualified Data.Word8 as W
import qualified Data.ByteString.Base64 as B64
import Data.Hex

isEvenLength :: String -> Bool
isEvenLength = even . length

makeEvenLength :: String -> String
makeEvenLength s
  | isEvenLength s = s
  | otherwise = "0" ++ s

{-hexStrInPairs :: String -> [String]-}
{-hexStrInPairs (c:[]) = [makeEvenLength [c]]-}
{-hexStrInPairs (c1:c2:[]) = [[c1] ++ [c2]]-}
{-hexStrInPairs (c1:c2:s) = [[c1] ++ [c2]] ++ hexStrInPairs s-}

hexStrInPairs :: String -> [String]
hexStrInPairs s = case isEvenLength s of false -> makePairs $ makeEvenLength s
                                         _ -> makePairs s
                       where makePairs (c1:c2:[]) = [[c1] ++ [c2]]
                             makePairs (c1:c2:s) = [[c1] ++ [c2]] ++ makePairs s

-- Converts a hex string representation of a byte into ByteString
byteStrToBytes :: String -> Maybe B.ByteString
byteStrToBytes str = case unhex str of Nothing -> Nothing
                                      Just s -> Just (C8.pack s)

bytesToStr :: B.ByteString -> String
bytesToStr = map (C.chr . fromEnum) . B.unpack

stringToBase64 :: String -> Maybe B.ByteString
stringToBase64 = encode' . byteStrToBytes
  where encode' Nothing = Nothing
        encode' (Just b) = Just (B64.encode b)

base64ToString :: B.ByteString -> String
base64ToString = bytesToStr . B64.decodeLenient

