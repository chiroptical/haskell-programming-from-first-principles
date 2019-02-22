module Constant where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant x' = Constant (x <> x')

instance Monoid a => Foldable (Constant a) where
    foldMap _ _ = mempty

instance Monoid a => Traversable (Constant a) where
    traverse _ (Constant x) = Constant <$> pure x
    sequenceA (Constant x) = Constant <$> pure x

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq