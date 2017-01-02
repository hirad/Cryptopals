import Shared

import qualified Data.ByteString as B
import Data.Bits (xor)
import Data.Hex (hex)

hexxor_ :: String -> String -> Maybe String
hexxor_ s1 s2 = hexxor' (byteStrToBytes s1) (byteStrToBytes s2)
  where hexxor' (Just bs1) (Just bs2) = Just(bytesToStr . B.pack $ B.zipWith xor bs1 bs2)
        hexxor' Nothing _ = Nothing
        hexxor' _ Nothing = Nothing

-- The challenge actually wants a hex string out, so this will do it:
hexxor :: String -> String -> Maybe String
hexxor s1 s2 = hex <$> hexxor_ s1 s2
