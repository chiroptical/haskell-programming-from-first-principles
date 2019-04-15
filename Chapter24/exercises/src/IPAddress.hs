module IPAddress where

import Control.Applicative
import Text.Trifecta
import Data.Word (Word32)
import Data.Bits (shift)

newtype IPAddress =
    IPAddress {ip :: Word32}
    deriving (Eq, Ord, Show)

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)

parseIPAddress :: Parser IPAddress
parseIPAddress = IPAddress <$> ipAddr
    where
        -- ipAddr :: Parser Word32
        -- ipAddr = genBinaryRep <$> pairWithLength
        -- genBinaryRep (xs, len) = foldr (\(exp, val) acc -> acc + val * 256 ^ exp) 0 $ zip [len - 1, len - 2 ..] xs
        -- pairWithLength = mapToSnd length <$> octets
        ipAddr :: Parser Word32
        ipAddr = foldr (\(s, v) acc -> shift v s + acc) 0 <$> (zip [24, 16, 8, 0] <$> octets)
        octet :: Parser [Char]
        octet = do
            xs <- some (oneOf "0123456789")
            case xs of
                ('0':_:_) -> fail "Octet can't have leading zero"
                _         -> return xs
        octets :: Parser [Word32]
        octets = do
            xs <- (fmap . fmap) read $ sepBy octet (char '.')
            case length xs of
                4 -> do
                    case all (<=255) xs of
                        True -> return xs
                        _ -> fail "IPAddress octets must be less than 255"
                _ -> fail "IPAddress must have 4 octets"