module IPAddress where

import Control.Applicative
import Text.Trifecta
import Data.Word

newtype IPAddress =
    IPAddress
    { ip :: Word32 }
    deriving (Eq, Ord, Show)

parseOctets :: Parser Int
parseOctets = octets
    where
        octet :: Parser [Char]
        octet = do
            xs <- some (oneOf "0123456789")
            case xs of
                ('0':_:_) -> fail "Octet can't have leading zero"
                _         -> return xs
        octets :: Parser Int
        octets = do
            xs <- sepBy octet (char '.')
            return $ length xs

parseIPAddress :: Parser IPAddress
parseIPAddress = IPAddress <$> ipAddr
    where
        ipAddr :: Parser Word32
        ipAddr = folder <$> lens
        folder = (\(xs, len) -> foldr (\(exp, val) acc -> acc + val * 256 ^ exp) 0 $ zip [len - 1, len - 2 ..] xs)
        lens = (\xs -> (xs, length xs)) <$> octets
        octet :: Parser [Char]
        octet = do
            xs <- some (oneOf "0123456789")
            case xs of
                ('0':_:_) -> fail "Octet can't have leading zero"
                _         -> return xs
        octets :: Parser [Word32]
        octets = do
            xs <- sepBy octet (char '.')
            case length xs of
                4 -> do
                    ys <- return $ read <$> xs
                    case all (<=255) ys of
                        True -> return ys
                        _ -> fail "IPAddress octets must be less than 255"
                _ -> fail "IPAddress must have 4 octets"