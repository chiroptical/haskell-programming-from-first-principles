module List where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Semigroup (List a) where
    xs <> Nil = xs
    Nil <> xs = xs
    Cons x xs <> ys = Cons x (xs <> ys)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = fmap f xs <> (fs <*> xs)

instance Foldable List where
    foldr _ acc Nil = acc 
    foldr f acc (Cons x xs) = f x $ foldr f acc xs
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
    traverse _ Nil = pure Nil
    -- g :: a -> f b
    -- x :: a
    -- xs :: List a
    -- traverse g xs :: f (List b)
    -- fmap pure (g x) :: f (List b)
    -- fmap (<>) (fmap pure (g x)) :: f (List b -> List b)
    -- traverse g (Cons x xs) = fmap (<>) (fmap pure (g x)) <*> traverse g xs
    traverse g (Cons x xs) = Cons <$> g x <*> traverse g xs
 
take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons x xs) = if n <= 0
                      then Nil
                      else Cons x (take' (n - 1) xs)
  
vectorOfList :: Arbitrary a => Int -> Gen (List a)
vectorOfList n = if n <= 0
                 then return Nil
                 else Cons <$> arbitrary <*> vectorOfList (n - 1)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized vectorOfList

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let l = xs in take' 100 l
          ys' = let l = ys in take' 100 l