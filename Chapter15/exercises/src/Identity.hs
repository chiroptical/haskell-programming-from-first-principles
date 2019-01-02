-- Chapter 15, Exercise 2
module Identity where

import Test.QuickCheck (Arbitrary, arbitrary)

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity x) (Identity x') = Identity $ x <> x'

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty