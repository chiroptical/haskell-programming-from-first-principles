-- Originally wrote this, but saw the recursive pattern
-- eftBool :: Bool -> Bool -> [Bool]
-- eftBool b e
--     | b < e = [b, e]
--     | b == e = [b]
--     | otherwise = []

eftBool :: Bool -> Bool -> [Bool]
eftBool b e
    | b < e = [b] ++ eftBool (succ b) e
    | b == e = [b]
    | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd b e
    | b < e = [b] ++ eftOrd (succ b) e
    | b == e = [b]
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt b e
    | b < e = [b] ++ eftInt (succ b) e
    | b == e = [b]
    | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar b e
    | b < e = [b] ++ eftChar (succ b) e
    | b == e = [b]
    | otherwise = []
