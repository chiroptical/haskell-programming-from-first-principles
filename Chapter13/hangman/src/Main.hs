module Main where

import Control.Monad (forever)
import Data.Char (toLower, isAlpha)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String]
  deriving (Eq, Show)

allAlphaWords :: IO WordList
allAlphaWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList $ filter (all isAlpha) $ lines dict

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxIncorrectGuesses :: Int
maxIncorrectGuesses = 9

gameLength :: String -> Bool
gameLength w = let l = length w
               in     l >= minWordLength
                   && l < maxWordLength

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allAlphaWords
  return $ WordList (filter gameLength aw)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed incorrectGuesses) =
    intersperse ' ' (map renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed ++ " Guesses left: "
    ++ show (maxIncorrectGuesses - incorrectGuesses)

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) "" 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar _ = '_'

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s ig) c goodBad =
  Puzzle word newFilledInSoFar (c : s) newGuessCount
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        newGuessCount = if goodBad then ig else ig + 1

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that, pick another character"
      return puzzle
    (True, _) -> do
      putStrLn "Good guess!"
      return (fillInCharacter puzzle guess True)
    (_, _) -> do
      putStrLn "Bad guess :(, try again!"
      return (fillInCharacter puzzle guess False)

gameOver :: Puzzle -> IO ()
gameOver puzz@(Puzzle wordToGuess _ guessed ig)
  | gameWin puzz = do
    putStrLn $ "You win! Word was: " ++ wordToGuess
    exitSuccess
  | ig == maxIncorrectGuesses = do
    putStrLn "You lost!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess
  | otherwise = return ()

gameWin :: Puzzle -> Bool
gameWin (Puzzle _ filledInSoFar _ _) =
  all isJust filledInSoFar

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  -- gameWin puzzle
  putStrLn $ "Current Puzzle: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> if isAlpha c then handleGuess puzzle c >>= runGame
           else putStrLn "Must submit one alpha character!"
    _ -> putStrLn "Must submit one alpha character!"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
