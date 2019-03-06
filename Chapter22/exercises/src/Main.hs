module Main where

import Data.Maybe
import ReaderPractice

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ summed <$> ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ bolt <$> z
  -- print $ sequenceA [(>3), (<8), even] 7

  print $ sequA 6
  print $ foldr (&&) True (sequA 6)
  print s'
  print $ sequA (fromMaybe 0 s')
  print ys
  print $ bolt (fromMaybe 0 ys)