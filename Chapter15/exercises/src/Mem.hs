-- Chapter 15, Monoid Exercise 8

module Mem where

import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary)
import MyTypes

newtype Mem s a = Mem { runMem :: s -> (a, s) }

-- > f = Mem $ \s -> ("hi", s + 1)
-- > runMem mempty 0 :: (String, Int)
-- ("", 0)
-- > runMem (f <> mempty) 0
-- ("hi", 1)
-- > runMem (mempty <> f) 0
-- ("hi", 1)

instance Show (Mem s a) where
  show _ = "_"

instance (Semigroup s, Semigroup a) => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem (\x -> f x <> g x)

instance (Monoid s, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ const (mempty, mempty)

isMemAssoc :: Mem String SI -> Mem String SI -> Mem String SI -> String -> Bool
isMemAssoc ma mb mc x = runMem ((ma <> mb) <> mc) x == runMem (ma <> (mb <> mc)) x

memLeftIdentLaw :: Mem String SI -> String -> Bool
memLeftIdentLaw ma x = runMem (mempty <> ma) x == runMem ma x

memRightIdentLaw :: Mem String SI -> String -> Bool
memRightIdentLaw ma x = runMem (ma <> mempty) x == runMem ma x

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary