{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

import Data.Char (toUpper)

import Data.List (find, elemIndex)
import Data.List.Split (splitOn)

import Data.Maybe (mapMaybe)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 50)

isCar :: Vehicle -> Bool
isCar (Car m p) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane a s) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m
getManu _ = error "Vehicle isn't a Car!"

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Integer where
    tooMany n = n > 42

newtype Goats = Goats Integer deriving (Eq, Show, TooMany)

-- GeneralizedNewtypeDeriving Pragma at top of file
-- instance TooMany Goats where
--    tooMany (Goats n) = tooMany n

-- FlexibleInstances Pragma at top of file
instance TooMany (Integer, String) where
    tooMany (x, _) = tooMany x

-- Another option without FlexibleInstances
-- newtype IString = IString (Integer, String) deriving (Eq, Show)
-- instance TooMany IString where
--     tooMany (IString (x, _)) = tooMany x

instance TooMany (Integer, Integer) where
    tooMany (x, x') = tooMany $ x + x'

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, x') = tooMany $ x + x'

-- data Fiction = Fiction deriving Show
-- data NonFiction = NonFiction deriving Show

-- data BookType = FictionBook Fiction | NonFictionBook NonFiction deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)

data Author' = Fiction AuthorName | NonFiction AuthorName deriving (Eq, Show)

-- data FlowerType = Daisy | Rose | Lilac deriving Show

type Gardener = String

-- Product of sums
-- data Garden = Garden Gardener FlowerType deriving Show

data Garden = Daisy Gardener | Rose Gardener | Lilac Gardener deriving (Eq, Show)

data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pFirst :: a, pSecond :: b } deriving (Eq, Show)

-- Working on my style

data OperatingSystem =
      Linux
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ Linux
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages =
    [ Haskell
    , Agda
    , Idris
    , PureScript
    ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = x, lang = y }
    | x <- allOperatingSystems, y <- allLanguages]

allProgrammers' :: [Programmer]
allProgrammers' = Programmer <$> allOperatingSystems <*> allLanguages

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 2 Leaf) 2 (Node Leaf 3 Leaf)

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node l a r) = [a] ++ (preOrder l) ++ (preOrder r)

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node l a r) = (preOrder l) ++ [a] ++ (preOrder r)

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node l a r) = (preOrder l) ++ (preOrder r) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node l x Leaf) = f x (foldTree f acc l)
foldTree f acc (Node Leaf x r) = f x (foldTree f acc r)
foldTree f acc (Node l x r) = foldTree f (f x (foldTree f acc r)) l

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf t@(x:xs) (y:ys)
    | x == y = isSubseqOf xs ys
    | otherwise = isSubseqOf t ys

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\xs -> (xs, capitalizeWord xs)) . words

concatSentence :: [String] -> String
concatSentence [] = []
concatSentence (x:[]) = capitalizeWord x
concatSentence (x:xs) = capitalizeWord x ++ ". " ++ concatSentence xs

capitalizeParagraph :: String -> String
capitalizeParagraph =  concatSentence . splitOn ". "

data Phone = Phone [(Char, String)] deriving (Eq, Show)

phone = Phone [ ('0', "+ 0")
              , ('1', "1")
              , ('2', "ABC2")
              , ('3', "DEF3")
              , ('4', "GHI4")
              , ('5', "JKL5")
              , ('6', "MNO6")
              , ('7', "PQRS7")
              , ('8', "TUV8")
              , ('9', "WXYZ9")
              , ('*', "^*")
              , ('#', ".,#")
              ]

type Digit = Char

digitToTaps :: Phone -> Digit -> Maybe String
digitToTaps (Phone l) c = replDigit (loc key) key
    where key = find (\(_, e) -> elem c e) l
          loc Nothing = Nothing
          loc (Just (_, s)) = elemIndex c s
          replDigit Nothing _ = Nothing
          replDigit _ Nothing = Nothing
          replDigit (Just n) (Just (d, _)) = Just $ replicate (n + 1) d

stringToTaps :: String -> Maybe [String]
stringToTaps = mapM (digitToTaps phone)

tapToChar :: Phone -> String -> Maybe Char
tapToChar (Phone l) xs = get key len
    where key = find (\(_, e) -> elem (head xs) e) l
          len = length xs - 1
          get Nothing _ = Nothing
          get (Just (_, e)) n = Just $ e !! n

tapsToString :: [String] -> Maybe String
tapsToString = mapM (tapToChar phone)
