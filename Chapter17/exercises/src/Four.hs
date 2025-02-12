module Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four m n o p) = Four m n o (f p)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    Four m n o fp <*> Four m' n' o' p = Four (m <> m') (n <> n') (o <> o') $ fp p

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            Four a b c <$> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq