module Main where

import           Lib
import           StoppingTheParty
import           WhySomeException

main :: IO ()
main = do
  -- print $ multiError 0
  -- print $ multiError 1
  -- print $ multiError 2
  runStoppingTheParty
