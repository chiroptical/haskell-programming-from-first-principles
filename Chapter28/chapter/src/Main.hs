module Main where

import           Criterion.Main
import Guarded
import Profiling
import HeapUsage

max' :: Ord a => [a] -> a
max' [] = error "Max of empty list"
max' [x] = x
max' (x:xs) =
  foldr
    (\a acc ->
       if a > acc
         then a
         else acc)
    x
    xs

infixl 9 !?

(!?) :: [a] -> Int -> Maybe a
{-# INLINABLE (!?) #-}
_ !? n
  | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

(!/) :: [a] -> Int -> Maybe a
{-# INLINABLE (!/) #-}
xs !/ n
  | n < 0 = Nothing
  | otherwise =
    foldr
      (\x r k ->
         case k of
           0 -> Just x
           _ -> r (k - 1))
      (const Nothing)
      xs
      n

myList :: [Int]
myList = [1 .. 9999]

-- main :: IO ()
-- main =
--   defaultMain
--     [ bench "naive 9999" $ whnf (myList !?) 9998
--     , bench "standard 9999" $ whnf (myList !/) 9998
    -- ]

main :: IO ()
main = do
  -- runGuarded    
  -- runProfiling
  runHeapUsage
