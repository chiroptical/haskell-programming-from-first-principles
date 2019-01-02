-- Chapter 15, Exercise 9
module Combine where

import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary)
import MyTypes

newtype Combine a b = Combine { unCombine :: a -> b }

instance Show (Combine a b) where
  show _ = "_"

-- > f = Combine $ \n -> Sum (n + 1)
-- > g = Combine $ \n -> Sum (n - 1)
-- > unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- > unCombine (f <> f) $ 1
-- Sum {getSum = 4}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

isCombineAssoc :: Combine Int SI -> Combine Int SI -> Combine Int SI -> Int -> Bool
isCombineAssoc ca cb cc x = unCombine ((ca <> cb) <> cc) x == unCombine (ca <> (cb <> cc)) x

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty

combineLeftIdentLaw :: Combine Int SI -> Int -> Bool
combineLeftIdentLaw ca x = unCombine (mempty <> ca) x == unCombine ca x

combineRightIdentLaw :: Combine Int SI -> Int -> Bool
combineRightIdentLaw ca x = unCombine (ca <> mempty) x == unCombine ca x