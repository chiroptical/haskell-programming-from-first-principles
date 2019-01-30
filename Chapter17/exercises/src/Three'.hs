module Three' where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Three' a b = Three' a a b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x x' y) = Three' x x' (f y)

instance Monoid a => Applicative (Three' a) where
    pure = Three' mempty mempty
    Three' x x' fz <*> Three' y y' z = Three' (x <> y) (x' <> y') (fz z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        Three' a a' <$> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq