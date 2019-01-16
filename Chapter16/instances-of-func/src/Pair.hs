module Pair where

import Test.QuickCheck

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        Pair x <$> arbitrary