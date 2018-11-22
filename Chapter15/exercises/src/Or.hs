-- Chapter 15, Exercise 8
module Or where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

data Or a b = Fst a | Snd b deriving (Eq, Show)

-- Fst 1 <> Snd 2 = Snd 2
-- Fst 1 <> Fst 2 = Fst 2
-- Snd 1 <> Fst 2 = Snd 1
-- Snd 1 <> Snd 2 = Snd 2
instance Semigroup (Or a b) where
  (<>) _ x@(Snd _) = x
  (<>) x@(Snd _) _ = x
  (<>) _ x@(Fst _) = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [ Fst <$> arbitrary
                    , Snd <$> arbitrary ]
