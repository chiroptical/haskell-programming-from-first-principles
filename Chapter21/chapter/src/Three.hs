module Three where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y $ f z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    Three x y f <*> Three x' y' z = Three (x <> x') (y <> y') (f z)

instance Foldable (Three a b) where
    foldMap f (Three _ _ z) = f z 

instance Traversable (Three a b) where
    traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three x y <$> arbitrary 

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq