module Big where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Big a b = Big a b b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where
    Big x y yy <> Big x' y' yy' = Big (x <> x') (y <> y') (yy <> yy') 

instance (Monoid a, Monoid b) => Monoid (Big a b) where
    mempty = Big mempty mempty mempty

instance Functor (Big a) where
    fmap f (Big x y yy) = Big x (f y) (f yy)

instance Monoid a => Applicative (Big a) where
    pure y = Big mempty y y
    Big x f ff <*> Big x' y yy = Big (x <> x') (f y) (ff yy)

instance Foldable (Big a) where
    foldMap f (Big _ y yy) = f y <> f yy

instance Traversable (Big a) where
    traverse f (Big x y yy) = Big x <$> f y <*> f yy

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Big x y <$> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq