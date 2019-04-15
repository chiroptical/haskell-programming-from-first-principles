module IPAddress6 where

import Control.Applicative
import Text.Trifecta
import Data.Word (Word64)
import Data.Bits (shift)
import Numeric (readHex)

newtype IPAddress6 =
    IPAddress6 {ip6 :: Integer}
    deriving (Eq, Show, Ord)

splitGroup :: [Char] -> [[Char]]
splitGroup [a, b, c, d] = [[a, b], [c, d]]

strPadLeft :: Char -> Int -> [Char] -> [Char]
strPadLeft c n inp =
    let lenInp = length inp
        maxChars = max 0 (n - lenInp)
        padStr = replicate maxChars c
    in padStr ++ inp

padIPv6 :: ([[Char]], Int) -> [[Char]]
padIPv6 (xs, 8) = xs
padIPv6 (xs, n) = go xs (8 - n + 1)
    where
        go :: [[Char]] -> Int -> [[Char]]
        go [l] _     = l : []
        go (l:ls) n' = if l == ""
                       then replicate n' (replicate 4 '0') ++ go ls n'
                       else l : go ls n'

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = IPAddress6 <$> ipAddr6
    where
        ipAddr6 :: Parser Integer
        ipAddr6 = foldr (\(s, v) acc -> shift v s + acc) 0 <$> (zip shiftValues <$> octets)
        shiftValues = reverse $ take 16 $ [0, 8..]
        group :: Parser [Char]
        group = do
            xs <- many hexDigit
            if length xs > 4 || length xs < 0
            then fail "Must have between 0 and 4 hexDigits in a group"
            else case length xs of
                    0 -> return []
                    _ -> return $ strPadLeft '0' 4 xs
        octets :: Parser [Integer]
        octets = do
            xs <- padIPv6 <$> (mapToSnd length <$> (sepBy group (char ':')))
            ys <- return $ splitGroup <$> xs
            zs <- return $ readHex <$> (ys >>= id)
            return $ foldr (\((x,_):_) acc -> x : acc) [] zs

