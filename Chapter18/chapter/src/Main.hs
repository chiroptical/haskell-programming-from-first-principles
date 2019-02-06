module Main where

import Control.Monad (join, (>=>))
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
  Second y >>= f = f y
  First x >>= _ = First x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [ First <$> arbitrary
                    , Second <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

forSumType :: Sum Int (Int, String, Integer)
forSumType = undefined

-- right identity
-- m >>= return = m

-- left identity
-- return x >>= f = f x

-- associativity
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe n x) = CountMe n (f x)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' x = CountMe (n + n') (f x)

instance Monad CountMe where
  return = pure
  CountMe n x >>= f =
    let CountMe n' x' = f x
    in CountMe (n + n') x'

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = do
    n <- arbitrary
    CountMe n <$> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

forCountMeType :: CountMe (Int, String, Integer)
forCountMeType = undefined

-- Kleisli Composition
monadComposition :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
monadComposition f g a = g a >>= f

-- flip (.) :: (a -> b) -> (b -> c) -> a -> c
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM 

-- f >=> g :: (a -> m c)

askForAge :: IO Int
askForAge = getAge "Hello, how old are you?"

main :: IO ()
main = do
  -- quickBatch $ functor forSumType
  -- quickBatch $ applicative forSumType
  -- quickBatch $ monad forSumType
  quickBatch $ functor forCountMeType
  quickBatch $ applicative forCountMeType
  quickBatch $ monad forCountMeType