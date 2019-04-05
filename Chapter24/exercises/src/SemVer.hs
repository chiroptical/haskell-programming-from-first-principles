{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import Control.Applicative
import Text.Trifecta
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

data NumberOrString =
      NOSS String
    | NOSI Integer
    deriving (Eq, Show, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Prerelease = [NumberOrString]
type Metadata = [NumberOrString]

instance Ord SemVer where
  compare x x' =
    comparing major x x' <>
    comparing minor x x' <>
    comparing patch x x' <>
    comparing prerelease x x'

  -- @Lumie1337
  -- Monoid is recursive for ((->) a b)
  -- compare =
  --   comparing major <>
  --   comparing minor <>
  --   comparing patch <>
  --   comparing prerelease

data SemVer =
    SemVer
    { major :: Major
    , minor :: Minor
    , patch :: Patch
    , prerelease :: Prerelease
    , metadata :: Metadata
    }
    deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$>
              versionElement <* char '.' <*>
              versionElement <* char '.' <*>
              versionElement <*> 
              prerelease <*>
              metadata
  where 
    optionally p = fromMaybe [] <$> optional p
    prerelease = optionally $ char '-' *> (numberOrString `sepBy` (symbol "."))
    metadata = optionally $ char '+' *> (numberOrString `sepBy` (symbol "."))
    versionElement = fmap read $ ((:) <$> oneOf "123456789" <*> many (oneOf "0123456789") <|> some (char '0'))
    numberOrString = (NOSI <$> integer) <|> (NOSS <$> some alphaNum)
