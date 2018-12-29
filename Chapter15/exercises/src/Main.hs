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

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity SI -> Identity SI -> Identity SI -> Bool)
  quickCheck (semigroupAssoc :: Two SI SI -> Two SI SI -> Two SI SI -> Bool)
  quickCheck (semigroupAssoc :: Three SI SI SI -> Three SI SI SI -> Three SI SI SI -> Bool)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: Or SI SD -> Or SI SD -> Or SI SD -> Bool)
  quickCheck isCombineAssoc
  quickCheck isCompAssoc
  quickCheck (semigroupAssoc :: Validation SI SD -> Validation SI SD -> Validation SI SD -> Bool)