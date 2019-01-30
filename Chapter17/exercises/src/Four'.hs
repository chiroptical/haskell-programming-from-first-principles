module Four' where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' m n o p) = Four' m n o (f p)

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    Four' m n o fp <*> Four' m' n' o' p = Four' (m <> m') (n <> n') (o <> o') $ fp p

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        x <- arbitrary
        x' <- arbitrary
        x'' <- arbitrary
        Four' x x' x'' <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq