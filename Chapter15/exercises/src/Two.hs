-- Chapter 15, Exercise 2
module Two where

import Test.QuickCheck (Arbitrary, arbitrary)

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    Two x <$> arbitrary

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
