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
