module Main where

import Data.Monoid
import Data.Foldable

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum
-- sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product
-- product' = foldr (*) 1

compareIfNotTrue :: (Eq a) => a -> a -> Bool -> Bool
compareIfNotTrue _ _ True = True
compareIfNotTrue x x' _ = x == x'

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (compareIfNotTrue x) False

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = getAny . foldMap (Any . (==x))

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Semigroup (Min a) where
  Min Nothing <> m = m
  n <> Min Nothing = n
  Min (Just x) <> Min (Just x')
    | x <= x' = Min $ Just x
    | otherwise = Min $ Just x'

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . foldMap (Min . Just)

-- This doesn't begin to enlighten the power of foldable
-- minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum'' xs = let go [] = Nothing
--                    go [x] = Just x
--                    go (x:xs) = Just $ foldr min x xs
--                in go $ toList xs

newtype Max a = Max { getMax :: Maybe a }

instance Ord a => Semigroup (Max a) where
  Max Nothing <> m = m
  n <> Max Nothing = n
  Max (Just x) <> Max (Just x')
    | x >= x' = Max $ Just x
    | otherwise = Max $ Just x'

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap (Max . Just)

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Integer
length' = foldr (\_ acc -> acc + 1) 0

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty

newtype Constant a b = Constant b

instance Semigroup b => Semigroup (Constant a b) where
    Constant y <> Constant y' = Constant $ y <> y'

instance Monoid b => Monoid (Constant a b) where
    mempty = Constant mempty

instance Foldable (Constant a) where
    foldMap f (Constant y) = f y

data Two a b = Two a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance Foldable (Two a) where
  foldMap f (Two a b) = f b
  -- foldr f z (Two a b) = f b z


filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
  
xs :: [Int]
xs = [2, 3, 4]

main :: IO ()
main = do
  print $ sum' xs
  print $ product' xs
  print $ elem'' 2 xs
  print $ elem'' 0 xs
