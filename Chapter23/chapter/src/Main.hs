module Main where

import System.Random
import Control.Monad.Trans.State
import Die
import Estado

-- newtype Reader r a = Reader { runReader :: r -> a }
-- newtype State s a = State { runState :: s -> (a, s) }

type Iso a b = (a -> b, b -> a)
newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

f :: Int -> Maybe Int
f x = Just x

g :: Maybe Int -> Int
g (Just x) = x

-- :: (a -> Maybe b, b -> Maybe a)
-- Not an isomorphism because it might not work, i.e. Nothing

-- :: [a] -> a, a -> [a]
-- Not an isomorphism because you can't undo the fold
-- [a] could be empty

main :: IO ()
main = do
  putStrLn "hello world"
