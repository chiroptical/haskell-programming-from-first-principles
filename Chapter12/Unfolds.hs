myIterate :: (a -> a) -> a -> [a]
myIterate f x = go f x
    where go f x = x : go f (f x)

myUnfoldR :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldR f x = go f x
    where go f x = 
