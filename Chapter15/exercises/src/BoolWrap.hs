-- Chapter 15, Exercise 6 & 7
module BoolWrap where

import Test.QuickCheck (Arbitrary, arbitrary)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

-- BoolConj True <> BoolConj True = True
-- BoolConj True <> BoolConj False = False

-- BoolDisj True <> BoolDisj True = True
-- BoolDisj True <> BoolDisj False = True

instance Semigroup BoolConj where
  (<>) _ x@(BoolConj False) = x
  (<>) x@(BoolConj False) _ = x
  (<>) _ _ = BoolConj True

instance Semigroup BoolDisj where
  (<>) _ x@(BoolDisj True) = x
  (<>) x@(BoolDisj True) _ = x
  (<>) _ _ = BoolDisj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance Monoid BoolConj where
  mempty = BoolConj True

instance Monoid BoolDisj where
  mempty = BoolDisj False