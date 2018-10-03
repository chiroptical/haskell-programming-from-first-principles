isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee x f (Just x') = f x'

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe (Nothing:xs) = Nothing
flipMaybe xs = foldl f (Just []) xs
    where f (Just y) (Just x) = Just $ y ++ [x]
          f _ Nothing = Nothing
          f _ _ = Nothing

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' [] = Nothing
flipMaybe' (Nothing:xs) = Nothing
flipMaybe' xs = foldr f (Just []) xs
    where f (Just x) (Just y) = Just $ (flip (:)) y x
          f _ Nothing = Nothing
          f _ _ = Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
    where f (Left x) y = x : y
          f _ y = y

rights' :: [Either a b] -> [b]
rights' = foldr f []
    where f (Right x) y = x : y
          f _ y = y

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = foldr f ([], [])
    where f (Left x) (l, r) = (x : l, r)
          f (Right x) (l, r) = (l, x: r)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' f (Left x) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f f' (Left x) = f x
either' f f' (Right x) = f' x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (\x -> Just $ f x) e
