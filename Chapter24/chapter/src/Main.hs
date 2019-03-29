{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBadFraction = "10"
shouldWork = "1/2"
shouldWorkToo = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  num <- decimal
  char '/'
  denom <- decimal
  case denom of
    0 -> fail "denominator of rational cannot be zero"
    _ -> return (num % denom)

parseFraction' = parseString parseFraction mempty

parseIntegerToEOF :: Parser Integer
parseIntegerToEOF = do
  int <- integer
  eof
  return int

testGood = "123"
testBad = "123abc"

type IntegerOrString = Either Integer String

testIOSOne = "blah"
testIOSTwo = "123"
testIOSThree = testIOSTwo ++ testIOSOne ++ "456"

parseIOS :: Parser IntegerOrString
parseIOS =
      (Left <$> integer)
  <|> (Right <$> some letter) 

p f i = parseString f mempty i

type DoR = Either Integer Rational

parseDoR :: Parser DoR
parseDoR =
      (Left <$> try parseIntegerToEOF)
  <|> (Right <$> try parseFraction)

main :: IO ()
main = do
  -- print $ parseFraction' shouldWork
  -- print $ parseFraction' shouldWorkToo
  -- print $ parseFraction' alsoBadFraction
  -- print $ parseFraction' badFraction
  -- print $ parseString parseIntegerToEOF mempty testGood
  -- print $ parseString parseIntegerToEOF mempty testBad
  -- print $ p (some letter) testIOSOne
  -- print $ p (integer) testIOSTwo
  -- print $ p parseIOS testIOSOne
  -- print $ p parseIOS testIOSTwo
  -- print $ p (many parseIOS) testIOSThree
  -- print $ p (some parseIOS) testIOSThree
  print $ p (many parseIOS) ""
  print $ p (some parseIOS) ""