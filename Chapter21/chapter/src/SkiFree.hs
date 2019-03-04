{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (EqProp a, EqProp (n a)) => EqProp (S n a) where
    (S x y) =-= (S p q) = (x =-= p) .&. (y =-= q)

instance Functor n => Functor (S n) where
    -- f :: a -> b
    -- f x :: b
    -- fmap _ _ :: S n b
    -- n :: n b
    fmap f (S n x) = S (f <$> n) (f x)

instance Applicative n => Applicative (S n) where
    pure x = S (pure x) x
    S nf f <*> S n x = S (nf <*> n) (f x)

instance Foldable n => Foldable (S n) where
    foldMap f (S n x) = foldMap f n <> f x

instance Traversable n => Traversable (S n) where
    -- f :: a -> f b
    -- n :: n a
    -- x :: a
    -- traverse f n :: f (n b)
    -- sequenceA :: n (f a) -> f (n a)
    -- f x :: f b
    -- f <$> n = n (f b)
    traverse f (S n x) = S <$> sequenceA (f <$> n) <*> f x