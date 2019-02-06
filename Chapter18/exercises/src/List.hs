module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers

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
  Cons f fs <*> xs = fmap f xs <> (fs <*> xs)
  -- fs <*> xs = concat' $ flatMap (\f -> Cons (f <$> xs) Nil) fs

instance Monad List where
    (>>=) = flip flatMap

take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons x xs) = if n <= 0
                      then Nil
                      else Cons x (take' (n - 1) xs)
  
vectorOfList :: Arbitrary a => Int -> Gen (List a)
vectorOfList n = if n <= 0
                 then return Nil
                 else Cons <$> Test.QuickCheck.arbitrary <*> vectorOfList (n - 1)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized vectorOfList

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let l = xs in take' 15 l
          ys' = let l = ys in take' 15 l

checkList :: List (Int, String, Integer)
checkList = undefined