import Data.Maybe (fromJust)

myIterate :: (a -> a) -> a -> [a]
myIterate = go
    where go f x = x : go f (f x)

myUnfoldR :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldR = go
    where go f x = a : go f b
            where (a, b) = fromJust (f x)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldR (\y -> Just (y, f y))
