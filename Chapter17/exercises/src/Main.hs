module Main where

import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Pair
import Two
import Three
import Three'
import Four
import Four'

-- []
-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]

-- IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- (,) c
-- pure :: b -> (a, b)
-- (<*>) :: (c, (a -> b)) -> (c, a) -> (c, b)

-- (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

type IIS = (Integer, Int, String)

main :: IO ()
main = do
  putStrLn "Is Pair applicative?"
  quickBatch $ applicative (undefined :: Pair IIS)
  putStrLn "Is Two applicative?"
  quickBatch $ applicative (undefined :: Two String IIS)
  putStrLn "Is Three applicative?"
  quickBatch $ applicative (undefined :: Three String String IIS)
  putStrLn "Is Three' applicative?"
  quickBatch $ applicative (undefined :: Three' String IIS)
  putStrLn "Is Four applicative?"
  quickBatch $ applicative (undefined :: Four String String String IIS)
  putStrLn "Is Four' applicative?"
  quickBatch $ applicative (undefined :: Four' String IIS)