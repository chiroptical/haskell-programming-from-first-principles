import Data.Maybe (fromJust, isNothing)

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a, b, a))
    -> a
    -> BinaryTree b
unfoldTree = go
    where go f x
            | isNothing (f x) = Leaf
            | otherwise = Node (go f a) b (go f a')
                where (a, b, a') = fromJust (f x)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree unfolder 0
    where unfolder x
            | x == n = Nothing
            | otherwise = Just (x + 1, x, x + 1)
