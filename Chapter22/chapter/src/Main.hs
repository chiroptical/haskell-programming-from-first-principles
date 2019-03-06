{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

boop = (*2)
doop = (+10)

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  c <- cap
  r <- rev
  return (c, r)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \c -> rev >>= \r -> return (c, r)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- (<$>) :: (a -> b) -> (r -> a) -> (r -> b)
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- ra :: r -> a
-- rb :: r -> b
myLiftA2 f ra rb = f <$> ra <*> rb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  -- pure = Reader . const
  pure a = Reader $ \r -> const a r
  -- Reader rab <*> Reader ra = Reader $ \r -> rab r $ ra r
  Reader rab <*> Reader ra = Reader (rab <*> ra)

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = bar (foo r) r

frootR :: Num a => [a] -> ([a], Int)
frootR = \r -> bar (foo r) r

-- m a -> (a -> m b) -> m b
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

instance Monad (Reader r) where
  return = pure
  Reader ra >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

getDogReader :: Reader Person Dog
-- Reader dogName :: Reader Person DogName
-- Reader address :: Reader Person Address
getDogReader = do
  dog <- Reader dogName
  add <- Reader address
  return $ Dog dog add

main :: IO ()
main = do
  putStrLn "hello world"
