module Main where

import Test.QuickCheck (quickCheck)
import MyTypes
import Trivial
import Identity
import Two
import Three
import BoolWrap
import Or
import Combine
import Comp
import Validation
import Mem

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a

main :: IO ()
main = do
  -- Trivial is Semigroup/Monoid? yes
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  -- Identity is Semigroup/Monoid? yes
  quickCheck (semigroupAssoc :: Identity SI -> Identity SI -> Identity SI -> Bool)
  quickCheck (monoidLeftIdentity :: Identity SI -> Bool)
  quickCheck (monoidRightIdentity :: Identity SI -> Bool)

  -- Two is Semigroup/Monoid? yes
  quickCheck (semigroupAssoc :: Two SI SI -> Two SI SI -> Two SI SI -> Bool)
  quickCheck (monoidLeftIdentity :: Two SI SI -> Bool)
  quickCheck (monoidRightIdentity :: Two SI SD -> Bool)

  -- quickCheck (semigroupAssoc :: Three SI SI SI -> Three SI SI SI -> Three SI SI SI -> Bool)

  -- BoolConj is Semigroup/Monoid? yes
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  -- BoolDisj is Semigroup/Monoid? yes
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  -- Or is Semigroup only
  quickCheck (semigroupAssoc :: Or SI SD -> Or SI SD -> Or SI SD -> Bool)
  -- quickCheck (monoidLeftIdentity :: Or SI SD -> Bool)
  -- quickCheck (monoidRightIdentity :: Or SI SD -> Bool)

  -- Combine is Semigroup/Monoid? yes
  quickCheck isCombineAssoc
  quickCheck combineLeftIdentLaw
  quickCheck combineRightIdentLaw

  -- Comp is Semigroup/Monoid? yes
  quickCheck isCompAssoc
  quickCheck compLeftIdentLaw
  quickCheck compRightIdentLaw

  quickCheck (semigroupAssoc :: Validation SI SD -> Validation SI SD -> Validation SI SD -> Bool)

  -- Mem is Semigroup/Monoid? yes
  quickCheck isMemAssoc
  quickCheck memLeftIdentLaw
  quickCheck memRightIdentLaw