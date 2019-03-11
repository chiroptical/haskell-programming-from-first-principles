module Main where

import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Die
import Estado
import Data.DList as DL

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

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod`  5 == 0 = "Buzz"
  | n `mod`  3 == 0 = "Fizz"
  | otherwise = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList xs = execState (mapM_ addResult xs) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo n n' = execState (mapM_ addResult' (reverse [n..n'])) []

addResult' :: Integer -> State [String] ()
addResult' n = do
  xs <- get
  let res = fizzBuzz n
  put (res : xs)

get' :: Estado s s
get' = Estado $ \r -> (r, r)

put' :: s -> Estado s ()
put' s = Estado $ \r -> ((), s)

exec' :: Estado s a -> s -> s
exec' (Estado sa) s = snd $ sa s

eval' :: Estado s a -> s -> a
eval' (Estado sa) = \r -> fst $ sa r

modify'' :: (s -> s) -> Estado s ()
modify'' ss = Estado $ \s -> ((), ss s)

main :: IO ()
main = do
  -- mapM_ putStrLn $ fizzBuzzList [1..100]
  -- mapM_ putStrLn $ fizzBuzzFromTo 1 100
  putStrLn "helloWorld"