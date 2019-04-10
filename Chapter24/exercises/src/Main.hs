module Main where

import Control.Applicative
import Text.Trifecta
import Data.Maybe

import SemVer
import PhoneNumber

semverEx :: String
semverEx = "1.0.0-gamma+002"

semverEx' :: String
semverEx' = "1.0.0-beta+oof.sha.41af286"

semverEx'' :: String
semverEx'' = "1.0.0-x.7.z.92"

p f i = parseString f mempty i

semver = p parseSemVer semverEx
semver' = p parseSemVer semverEx'

big = SemVer 2 1 1 [] []
little = SemVer 2 1 0 [] []

parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  neg <- optional $ char '-'
  int <- base10Integer
  return $ case neg of
    Just '-' -> negate int
    _        -> int

valid = "123-456-7890"
valid' = "1234567890"
valid'' = "(123) 456 7890"
valid''' = "+1-123-456-7890"
valid'''' = "+52-123-456-7890"
valid''''' = "(123) 456-7890"

invalid = "(332-222-4444"
invalid' = "+1-123-456 7890"

main :: IO ()
main = do
  -- print semver
  -- print semver'
  -- print $ (<) <$> semver <*> semver'
  -- print $ (NOSI 1) > (NOSS "hello")
  -- print $ big > little
  -- print $ parseString parseDigit mempty "123"
  -- print $ parseString parseDigit mempty "abc"
  -- print $ parseString base10Integer' mempty "123abc"
  -- print $ parseString base10Integer' mempty "abc"
  -- print $ parseString base10Integer' mempty "-123abc"
  print $ parseString parsePhoneNumber'' mempty valid
  print $ parseString parsePhoneNumber'' mempty valid'
  print $ parseString parsePhoneNumber'' mempty valid''
  print $ parseString parsePhoneNumber'' mempty valid'''
  print $ parseString parsePhoneNumber'' mempty valid''''
  print $ parseString parsePhoneNumber'' mempty valid'''''
  print $ parseString parsePhoneNumber'' mempty invalid
  print $ parseString parsePhoneNumber'' mempty invalid'

