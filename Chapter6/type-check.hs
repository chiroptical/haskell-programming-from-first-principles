import Data.List (sort, genericReplicate)

-- 1. --
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. --
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot
               then Blah
               else x

-- 3. --
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given datatype declarations, can we? --
data Rocks = Rocks String deriving (Eq, Ord, Show)
data Yeah = Yeah Bool deriving (Eq, Ord, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

-- 1. --
phew = Papu (Rocks "chases") (Yeah True)

-- 2. --
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. --
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4. --
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types --
-- 1. --
i :: Num a => a
i = 1

-- 2. --
f :: Fractional a => a
f = 1.0

-- 4. --
g :: RealFrac a => a
g = 1.0

-- 5. --
freud :: Ord a => a -> a
freud x = x

-- 6. --
freud' :: Int -> Int
freud' x = x

-- 7. --
-- myX = 1 :: Int
-- sigmund :: a -> a
-- sigmund x = myX

-- 8. --
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

-- 9. --
jung :: [Int] -> Int
jung xs = head (sort xs)

-- Type-Kwon-Do --

-- 1. --
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

-- 2. --
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB n a = foldl1 (+) $ map aToB (genericReplicate n a)
