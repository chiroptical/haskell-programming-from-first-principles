-- Chapter 15, Exercise 11
module Validation where

import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, oneof)
import MyTypes

-- > Success 1 <> failure "blah"
-- Success 1
-- > Success 1 <> Success 2
-- Success 1
-- > Failure "blah" <> Failure "blah"
-- Failure "blahblah"

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) x@(Success _) _ = x
  (<>) _ x@(Success _) = x
  (<>) (Failure x) (Failure x') = Failure $ x <> x'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [ Success <$> arbitrary
                    , Failure <$> arbitrary]