module Tree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf x) = f x
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
    -- :: (a -> f b) -> t a -> f (t b)
    traverse _ Empty = pure Empty
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Node l x r) =
        let l' = traverse f l
            r' = traverse f r
        in Node <$> l' <*> f x <*> r'

insertTree :: Ord a => Tree a -> Tree a -> Tree a
insertTree Empty Empty = Empty
insertTree xs Empty = xs
insertTree Empty xs = xs
insertTree (Leaf x) (Leaf x')
    | x < x' = Node Empty x (Leaf x')
    | otherwise = Node (Leaf x) x' Empty
insertTree leaf@(Leaf x) (Node l x' r)
    | x < x' = Node (insertTree leaf l) x' r
    | otherwise = Node l x' (insertTree leaf r)
insertTree ns@(Node l x r) leaf@(Leaf x') = insertTree leaf ns
insertTree ns@(Node l x r) (Node l' x' r')
    | x < x' = Node (insertTree ns l') x' r'
    | otherwise = Node l' x' (insertTree ns r')

vectorOfTree :: (Arbitrary a, Ord a) => Int -> Gen (Tree a)
vectorOfTree n
    | n <= 0 = return Empty
    | otherwise = insertTree <$> (Leaf <$> arbitrary) <*> vectorOfTree (n - 1)

instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
    arbitrary = sized vectorOfTree

takeTree :: Int -> Tree a -> Tree a
takeTree _ Empty = Empty
takeTree _ (Leaf x) = Leaf x
takeTree n (Node l x r)
    | n <= 0 = Empty
    | otherwise = Node (takeTree (n - 1) l) x (takeTree (n - 1) r)

instance (Eq a) => EqProp (Tree a) where
    ts =-= ts' = xs `eq` xs'
        where xs  = let l = ts  in takeTree 10 l
              xs' = let l = ts' in takeTree 10 l