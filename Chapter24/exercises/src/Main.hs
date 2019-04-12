{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Applicative
import Text.Trifecta
import Data.Maybe
import Text.RawString.QQ

import SemVer
import PhoneNumber
import Activities
import IPAddress

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

-- valid = "123-456-7890"
-- valid' = "1234567890"
-- valid'' = "(123) 456 7890"
-- valid''' = "+1-123-456-7890"
-- valid'''' = "+52-123-456-7890"
-- valid''''' = "(123) 456-7890"

-- invalid = "(332-222-4444"
-- invalid' = "+1-123-456 7890"

logFile :: String
logFile = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover -- hello world
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-- IPv4 Parser
valid = "192.168.0.1"
valid' = "172.16.254.1"
valid'' = "204.120.0.15"

invalid = "00.00.00.00"
invalid' = "010.010.010.010"

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
  -- print $ parseString parsePhoneNumber'' mempty valid
  -- print $ parseString parsePhoneNumber'' mempty valid'
  -- print $ parseString parsePhoneNumber'' mempty valid''
  -- print $ parseString parsePhoneNumber'' mempty valid'''
  -- print $ parseString parsePhoneNumber'' mempty valid''''
  -- print $ parseString parsePhoneNumber'' mempty valid'''''
  -- print $ parseString parsePhoneNumber'' mempty invalid
  -- print $ parseString parsePhoneNumber'' mempty invalid'
  -- print $ parseString parseLogFile mempty logFile
  -- print $ parseString parseOctets mempty valid
  -- print $ parseString parseOctets mempty valid'
  -- print $ parseString parseOctets mempty valid''
  -- print $ parseString parseOctets mempty invalid
  -- print $ parseString parseOctets mempty invalid'
  -- print "----------------------------------------"
  print $ parseString parseIPAddress mempty valid
  print $ parseString parseIPAddress mempty valid'
  print $ parseString parseIPAddress mempty valid''
  print $ parseString parseIPAddress mempty invalid
  print $ parseString parseIPAddress mempty invalid'