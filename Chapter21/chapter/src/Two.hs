module Two where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    Two x f <*> Two x' y = Two (x <> x') $ f y

instance Foldable (Two a) where
    foldMap f (Two _ y) = f y

instance Traversable (Two a) where
    traverse f (Two x y) = Two x <$> f y 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        Two x <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq
