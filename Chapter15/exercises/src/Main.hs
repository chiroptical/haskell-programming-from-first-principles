module Main where

import Data.Semigroup (Sum)
import Test.QuickCheck (quickCheck)
import Trivial
import Identity
import Two
import Three
import BoolWrap
import Or

type SI = Sum Int
type SD = Sum Double

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
