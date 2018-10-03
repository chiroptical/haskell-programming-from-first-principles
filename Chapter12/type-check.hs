import Data.List (intercalate)

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just $ n + 2 else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show, Eq)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
    | name /= "" && age >= 0 = Just $ Person name age
    | otherwise = Nothing

data PersonInvalid = NameEmpty | AgeTooLow deriving (Show, Eq)

mkPerson' :: Name -> Age -> Either [PersonInvalid] Person
mkPerson' name age
    | name /= "" && age >= 0 = Right $ Person name age
    | name == "" && age < 0 = Left [NameEmpty, AgeTooLow]
    | age < 0 = Left [AgeTooLow]
    | otherwise = Left [NameEmpty]

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
    | age >= 0 = Right age
    | otherwise = Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
    | name /= "" = Right name
    | otherwise = Left [NameEmpty]

type ValidatePerson a = Either [PersonInvalid] a

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = validatePerson (nameOkay name) (ageOkay age)

validatePerson :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
validatePerson (Right name) (Right age) = Right $ Person name age
validatePerson (Left name) (Left age) = Left $ name ++ age
validatePerson _ (Left age) = Left age
validatePerson (Left name) _ = Left name

-- Exercises

notThe :: String -> Maybe String
notThe w
    | w /= "the" = Just w
    | otherwise = Nothing

replaceThe :: String -> String
replaceThe = intercalate " " . map f . words
    where f a
            | notThe a == Nothing = "a"
            | otherwise = a

replaceThe' :: String -> String
replaceThe' [] = []
replaceThe' xs@(_:[]) = xs
replaceThe' xs@(_:_:[]) = xs
replaceThe' xs@(a:b:c:ds)
    | notThe (a:b:c:[]) == Nothing = 'a' : replaceThe' ds
    | otherwise = a : replaceThe' (tail xs)

replaceThe'' :: String -> String
replaceThe'' = intercalate " " . go . map notThe . words
    where go [] = []
          go (Just x : []) = x : []
          go (_ : []) = "a" : []
          go (Just x : Just y : ys) = x : y : go ys
          go (_ : Just x : ys) = "a" : x : go ys
          go (Just x : _ : ys) = x : "a" : go ys
          go (_ : _ : ys) = "a" : "a" : go ys

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . map notThe . words
    where go [] = 0
          go (_: []) = 0
          go (Nothing : Just x: ys)
            | head x `elem` "aeiouy" = 1 + go ys
            | otherwise = 0 + go ys
          go (_ : x : xs) = go (x:xs)

data Vowel = A | E | I | O | U | Y deriving (Show, Eq)

mkVowel :: Char -> Maybe Vowel
mkVowel 'a' = Just A
mkVowel 'e' = Just E
mkVowel 'i' = Just I
mkVowel 'o' = Just O
mkVowel 'u' = Just U
mkVowel 'y' = Just Y
mkVowel _ = Nothing

countVowels :: String -> Integer
countVowels = go . map mkVowel
    where go [] = 0
          go (Just x : []) = 1
          go (Just x : xs) = 1 + go xs
          go (_:xs) = go xs

newtype Word' = Word' String deriving (Eq, Show)

-- go adds 1 for vowels and subtracts 1 for consonants
-- -> positive values are considered invalid
mkWord :: String -> Maybe Word'
mkWord s = valid . go . map mkVowel $ s
    where go [] = 0
          go (Just x : []) = 1
          go (Just x : xs) = (go xs) + 1
          go (_ : xs) = (go xs) - 1
          valid a
            | a > 0 || s == [] = Nothing
            | otherwise = Just (Word' s)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ Zero) = 1
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat a
    | a < 0 = Nothing
    | otherwise = Just $ go a
    where go a
            | a == 0 = Zero
            | otherwise = Succ (go $ a - 1)
