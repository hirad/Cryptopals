import Shared

import Data.Word8
import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.Char as C
import Data.List (elemIndex, sort, map)
import Data.Maybe (fromJust)

possibleKeys = [(minBound :: Word8) ..]

commons :: [Char]
commons = ['E', 'T', 'A', 'O', 'I', 'N', 'S', 'H', 'R', 'D', 'L', 'U']

isCommon :: Char -> Bool
isCommon c
  | C.isLower c = isCommon $ C.toUpper c
  | otherwise = elem c commons

decryptXORWithKey :: String -> Word8 -> Maybe String
decryptXORWithKey msg key = bytesToStr <$> B.map (xor key) <$> (byteStrToBytes msg)

scoreDecryption :: String -> Int
scoreDecryption s = foldl (\acc c -> acc + scoreChar c) 0 s
  where scoreChar c
          | isCommon c = case elemIndex (C.toUpper c) commons of Nothing -> error "Invalid"
                                                                 Just i -> length commons - i
          | C.isUpper c = 1
          | C.isLower c = 2
          | C.isNumber c = 0
          | C.isPunctuation c = -1
          | otherwise = -3

data Candidate = Candidate { text :: String,
                             score :: Int } deriving (Eq, Show)

instance Ord Candidate where
  (Candidate _ s1) `compare` (Candidate _ s2) = s1 `compare` s2

decryptXOR :: String -> String
decryptXOR s = text . head . sort $ map (toCandidates . fromJust) ([decryptXORWithKey s] <*> possibleKeys)
  where toCandidates s = Candidate s (scoreDecryption s)

