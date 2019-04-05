{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import Control.Applicative
import Text.Trifecta
import Data.Maybe

data NumberOrString =
      NOSS String
    | NOSI Integer
    deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type PreRelease = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer Major Minor Patch PreRelease Metadata
    deriving Show

-- Monadic Context
-- parseSemVer :: Parser SemVer
-- parseSemVer = do
--     major <- parseNoLeadingZero <* char '.'
--     minor <- parseNoLeadingZero <* char '.'
--     patch <- parseNoLeadingZero
--     prerelease <- optional (char '-' *> (parseNumberOrString `sepBy` (symbol ".")))
--     metadata <- optional (char '+' *> (parseNumberOrString `sepBy` (symbol ".")))
--     return $ SemVer major minor patch (fromMaybe [] prerelease) (fromMaybe [] metadata)

parseSemVer :: Parser SemVer
parseSemVer =
    SemVer <$>
    major <* char '.' <*>
    minor <* char '.' <*>
    patch <*> 
    prerelease <*>
    metadata
    where major = semVerElement
          minor = semVerElement
          patch = semVerElement

          prerelease =
            fromMaybe [] <$>
            optional (
                char '-' *> 
                (semVerPreRelease `sepBy` (symbol "."))
            )
          metadata = fromMaybe [] <$> optional (char '+' *> (semVerMetadata `sepBy` (symbol ".")))
          semVerElement = do
            s <- some (oneOf "0123456789")
            case s of
                ('0':_:_) -> fail "Found leading zero!"
                _ -> return $ read s
          semVerPreRelease = (NOSI <$> integer) <|> (NOSS <$> some alphaNum)
          semVerMetadata = (NOSI <$> integer) <|> (NOSS <$> some alphaNum)
