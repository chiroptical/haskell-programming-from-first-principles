module Main where

import Control.Applicative
import Text.Trifecta

import SemVer

semverEx :: String
semverEx = "1.0.0-gamma+002"

semverEx' :: String
semverEx' = "1.0.0-beta+oof.sha.41af286"

semverEx'' :: String
semverEx'' = "1.0.0-x.7.z.92"

p f i = parseString f mempty i

semver = p parseSemVer semverEx
semver' = p parseSemVer semverEx'

main :: IO ()
main = do
  print semver
  -- print $ (<) <$> semver <*> semver'
