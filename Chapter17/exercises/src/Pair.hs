module Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
    pure x = Pair x x
    (Pair f f') <*> (Pair x x') = Pair (f x) (f' x')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        Pair a <$> arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq