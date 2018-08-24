factorial :: (Ord a, Num a, Show a) => a -> a
factorial x
    | x < 0 = error $ "Factorial called with negative number: " ++ (show x)
    | x == 0 = 1
    | otherwise = x * factorial (x - 1)

fib :: (Eq a, Num a) => a -> a
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d c
            | n < d = (c, n)
            | otherwise = go (n - d) d (c + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "whoops"

frappe :: String -> String
frappe = flippy "haha"

addUp :: (Eq a, Num a) => a -> a
addUp n
    | n == 0 = 0
    | otherwise = n + addUp (n - 1)

multByAdd :: (Integral a) => a -> a -> a
multByAdd x 0 = 0
multByAdd x n = x + multByAdd x (n - 1)

dividedBy' :: Integral a => a -> a -> Maybe (a, a)
dividedBy' num denom = go num denom 0
    where go n d c
            | d == 0 = Nothing
            | n == 0 = Just (0, 0)
            | n < 0 && n > d = Just (c, n)
            | n > 0 && n < d = Just (c, n)
            | n > 0 && d < 0 = go (n + d) d (c - 1)
            | n < 0 && d > 0 = go (n + d) d (c - 1)
            | otherwise = go (n - d) d (c + 1)

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 . mc91 $ n + 11
