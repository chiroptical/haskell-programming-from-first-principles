module Nope where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Nope a = Nope deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = Nope

instance Applicative Nope where
    pure _ = Nope
    _ <*> _ = Nope

instance Monad Nope where
    return = pure
    _ >>= _ = Nope

instance Arbitrary (Nope a) where
    arbitrary = return Nope

instance EqProp (Nope a) where
    (=-=) = eq

checkNope :: Nope (Int, String, Integer)
checkNope = undefined