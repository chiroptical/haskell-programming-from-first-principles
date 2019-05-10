module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Random (randomRIO)

-- Rules:
-- User picks evens or odds
-- User picks 0 to 5
-- Computer picks randomly from 0 to 5
-- Sum if even = even wins
-- Sum if off = odd wins
-- First to 6 points wins


data Player =
    Human
  | Computer
  deriving Show

data Game =
  Game
    { human :: Integer
    , computer :: Integer
    } deriving Show

-- This should be Either so we can gracefully deal with errors
handleGuess :: String -> Maybe Integer
handleGuess "0" = Just 0
handleGuess "1" = Just 1
handleGuess "2" = Just 2
handleGuess "3" = Just 3
handleGuess "4" = Just 4
handleGuess "5" = Just 5
handleGuess _ = error "You only have 5 fingers!"

playGame :: StateT Game IO ()
playGame = do
  game <- get
  liftIO $ putStrLn "Guess a number between 0 and 5"
  guess' <- liftIO $ getLine
  let humanGuess = handleGuess guess'
      computerGuess = randomRIO (0 :: Integer, 5)
  undefined


main :: IO ()
main = do
  putStrLn "Choose evens or odds"
  evensOdds <- getLine
  if (evensOdds == "evens" || evensOdds == "odds")
  then putStrLn $ "You are the " ++ evensOdds ++ " player, good luck!"
  else error "Must choose evens or odds"
  results <- execStateT playGame $ Game 0 0
  print results