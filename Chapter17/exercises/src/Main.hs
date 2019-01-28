module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Pair
import Two

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

type IIS = (Integer, Int, String)

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Pair IIS)
  quickBatch $ applicative (undefined :: Two String IIS)