{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Data.ByteString
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ

import Tokenizer (main')
import Backtrack (main'')
import Marshall (marshall)

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

parseFraction' = parseString Main.parseFraction mempty

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
  <|> (Right <$> try Main.parseFraction)

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1\nblah=2"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx :: ByteString
commentEx = "; last modified 1 April 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n; hah"

commentEx'' :: ByteString
commentEx'' = "# blah\n# woot\n# hah"

skipComments :: Parser ()
skipComments =
  skipMany (do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL)

sectionEx :: ByteString
sectionEx = "; ignore this\n[states]\nchris=texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
chris=texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=wiki

[background]
color=white
|]

data Section = Section Header Assignments
    deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
    deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
    Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseINI :: Parser Config
parseINI = do
  sections <- some parseSection
  let mapOfSections = Prelude.foldr rollup M.empty sections
  return (Config mapOfSections)

-- Text -> Structure -> Meaning
-- parse -> unmarshall

-- Meaning -> Structure -> Text
-- marshall -> serialization

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