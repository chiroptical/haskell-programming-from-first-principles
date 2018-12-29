-- Chapter 15, Exercise 10
module Comp where

import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary)
import MyTypes

newtype Comp a = Comp { unComp :: a -> a }

instance Show (Comp a) where
  show _ = "_"

-- > f = Combine $ \n -> (n + 1)
-- > g = Combine $ \n -> (n - 1)
-- > unComp (f <> g) $ 0
-- 0
-- > unComp (f <> f) $ 1
-- 4

instance Semigroup a => Semigroup (Comp a) where
  (<>) (Comp f) (Comp f') = Comp (\x -> f x <> f' x)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

isCompAssoc :: Comp SI -> Comp SI -> Comp SI -> SI -> Bool
isCompAssoc ca cb cc x = unComp ((ca <> cb) <> cc) x == unComp (ca <> (cb <> cc)) x