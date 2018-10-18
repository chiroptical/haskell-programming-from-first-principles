module Main where

import Text.Read (readMaybe)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $
    PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "What is the name of the person?"
  name <- getLine
  putStrLn "What is the age of the person?"
  age <- getLine
  case readMaybe age :: Maybe Integer of
    Just x -> case mkPerson name x of
      Right p@(Person _ _) -> putStrLn $ "I made the person: " ++ show p
      Left NameEmpty -> putStrLn "I didn't catch that name?"
      Left AgeTooLow -> putStrLn "The age needs to be greater than 0!"
      Left p@(PersonInvalidUnknown _) -> putStrLn $ "Error unknown: " ++ show p
    _ -> putStrLn "Age must be an integer!"

main :: IO ()
main = gimmePerson
