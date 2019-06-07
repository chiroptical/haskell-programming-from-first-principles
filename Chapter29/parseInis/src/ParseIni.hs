module ParseIni where

import           Control.Applicative
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Text.Trifecta

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

type Name = String

type Value = String

type Assignments = Map Name Value

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

skipComments :: Parser ()
skipComments =
  skipMany
    (do _ <- char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseINI :: Parser Config
parseINI = do
  sections <- some parseSection
  let mapOfSections = Prelude.foldr rollup M.empty sections
  return (Config mapOfSections)
