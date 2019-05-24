module Main where

import StrictList

x = undefined
y = "blah"


main :: IO ()
main = do
  -- print $ take' 10 $ map' (+1) (repeat' 1)
  -- print $ take' 10 $ (repeat' 1)
  print $ x `seq` snd (x, y)
