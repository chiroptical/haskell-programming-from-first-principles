module Guarded where

import Criterion.Main

myList :: [Int]
myList = [1..9999]

runGuarded :: IO ()
runGuarded = defaultMain
  [ bench "map list 9999" $ nf (map (+1)) myList ]