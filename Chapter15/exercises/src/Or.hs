-- Chapter 15, Exercise 8
module Or where

import Test.QuickCheck (Arbitrary, arbitrary, oneof, frequency)

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
  -- arbitrary = frequency [ (1, Fst <$> arbitrary)
  --                       , (1, Snd <$> arbitrary) ]

-- Fails Right Identity Law
-- instance (Monoid a) => Monoid (Or a b) where
--   mempty = Fst mempty

-- Fails Right and Left Identity Law
-- instance (Monoid b) => Monoid (Or a b) where
--   mempty = Snd mempty