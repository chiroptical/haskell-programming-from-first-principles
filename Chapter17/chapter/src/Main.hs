{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List (elemIndex)
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups
-- Use pure, <$>, or <*> to type check

-- zip [1, 2, 3] [4, 5, 6] = a = [(1, 4), (2, 5), (3, 6)]
-- lookup 3 a = Just 6
-- Just 9
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- Just (6, 5)
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]

b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]

-- elemIndex 3 [1, 2, 3, 4, 5] == 2
-- liftA2 max (Just 2) (Just 3) == Just 3
maxed :: Maybe Int
maxed = max <$> a <*> b

xs = [1, 2, 3]
ys = [4, 5, 6]

m :: Maybe Integer
m = lookup 3 $ zip xs ys 

n :: Maybe Integer
n = lookup 2 $ zip xs ys

-- m = Just 6, n = Just 5
-- Just (6, 5)
summed :: Maybe Integer
summed = fmap sum $ (,) <$> m <*> n

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant _ <*> Constant x = Constant x

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

-- Exercise: Fixer Upper, use <$>, <*> and pure to make the code work
-- 1.

one = const <$> Just "hello" <*> pure "world"

two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = oneof [ return Fools
                    , return Twoo]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where
  (=-=) = eq

instance Monoid a => Semigroup (ZipList a) where
  (<>) = liftA2 mappend

instance Monoid a => Monoid (ZipList a) where
  -- mempty = ZipList []
  mempty = pure mempty

-- instance Arbitrary a => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> ys = ys
  Cons x xs <> ys = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

fold :: (a -> b -> b) -> b -> List a -> b
fold _ acc Nil = acc
fold f acc (Cons x xs) = f x $ fold f acc xs 

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ f <$> as

-- fmap :: (a -> b) -> List a -> List b
-- (<*>) :: List (a -> b) -> List a -> List b
instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  -- Cons f fs <*> xs = fmap f xs <> (fs <*> xs)
  fs <*> xs = concat' $ flatMap (\f -> Cons (f <$> xs) Nil) fs

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let l = xs in take' 15 l
          ys' = let l = ys in take' 15 l

vectorOfList :: (Arbitrary a) => Int -> Gen (List a)
vectorOfList n = if n <= 0
                 then return Nil
                 else Cons <$> arbitrary <*> vectorOfList (n - 1)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized vectorOfList

take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons x xs) = if n <= 0
                      then Nil
                      else Cons x (take' (n - 1) xs)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 15 l
          ys' = let (ZipList' l) = ys in take' 15 l

instance Semigroup (ZipList' a) where
  ZipList' Nil <> ys = ys
  ZipList' xs <> ZipList' ys = ZipList' $ xs <> ys

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ f <$> xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' (Cons f fs)  <*> ZipList' (Cons x xs) = ZipList' (Cons (f x) Nil) <> (ZipList' fs <*> ZipList' xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

type IIS = (Int, Integer, String)

data Validation e a = Fail e | Succ a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Fail x) = Fail x
  fmap f (Succ y) = Succ $ f y

instance Monoid e => Applicative (Validation e) where
  pure = Succ
  Fail f <*> Fail x = Fail (f <> x)
  Fail f <*> _ = Fail f
  _ <*> Fail x = Fail x
  Succ f <*> Succ y = Succ (f y)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Fail <$> arbitrary, Succ <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ semigroup (Nil :: List Int)
  quickBatch $ functor (Nil :: List IIS)
  quickBatch $ applicative (Nil :: List IIS)
  quickBatch $ semigroup (ZipList' Nil :: ZipList' (List Int))
  quickBatch $ applicative (ZipList' Nil :: ZipList' IIS)
  quickBatch $ applicative (undefined :: Validation String IIS)