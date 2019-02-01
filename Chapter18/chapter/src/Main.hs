module Main where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- (>>=)
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ f <$> ma

-- ma = [Int] 
-- f = Int -> [Int]
-- f <$> ma = [[Int]]
-- join (m (m a)) = m a = [Int] 

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

binding'' :: IO (IO ())
binding'' = putStrLn <$> getLine

twoBinds :: IO ()
twoBinds =
  putStrLn "Enter your name: " >>
  getLine >>= \name ->
    putStrLn "Enter your age:" >>
    getLine >>= \age ->
      putStrLn ("Hello " ++ name ++ ", your old at " ++ age ++ " years")

twoBinds' :: IO ()
twoBinds' = do
  name <- getLine
  age <- getLine
  putStrLn ("Hello " ++ name ++ ", your old at " ++ age ++ " years")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x, x]
    else [x]

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmptyString :: String -> Maybe String
noEmptyString "" = Nothing
noEmptyString xs = Just xs

noNegativeInt :: Int -> Maybe Int
noNegativeInt n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name' age' weight' = do
  name <- noEmptyString name'
  age <- noNegativeInt age'
  weight <- noNegativeInt weight'
  weightCheck $ Cow name age weight

mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name' age' weight' =
  noEmptyString name' >>= \name ->
    noNegativeInt age' >>= \age ->
      noNegativeInt weight' >>= \weight ->
        weightCheck $ Cow name age weight

-- Think Applicative
-- doSomething = do
--   a <- f
--   b <- g
--   c <- h
--   pure (a, b, c)

-- Think Monad
-- doSomething = do
--   a <- f n
--   b <- g m
--   c <- h o
--   pure (a, b, c)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second $ f y

instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  _ <*> First x = First x
  Second f <*> Second y = Second $ f y

instance Monad (Sum a) where
  return = pure
  First x >>= _ = First x
  Second y >>= f = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [ First <$> arbitrary
                    , Second <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

forSumType :: Sum Int (Int, String, Integer)
forSumType = undefined

main :: IO ()
main = do
  quickBatch $ functor forSumType
  quickBatch $ applicative forSumType
  quickBatch $ monad forSumType