module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           System.Random             (randomRIO)

-- Rules:
-- User picks evens or odds
-- User picks 0 to 5
-- Computer picks randomly from 0 to 5
-- Sum if even = even wins
-- Sum if odd = odd wins
-- First to 6 points wins
data EvenOdd
  = Even
  | Odd
  deriving (Show, Eq)

data Game =
  Game
    { humanScore    :: Integer
    , computerScore :: Integer
    , evenOdd       :: EvenOdd
    , message       :: String
    }

instance Show Game where
  show g =
    concat
      [ "Morra ("
      , show (evenOdd g)
      , " wins) Scores:\n"
      , "Yours: "
      , show (humanScore g)
      , "\n"
      , "Computers: "
      , show (computerScore g)
      , "\n"
      ]

-- This should be Either so we can gracefully deal with errors
parseGuess :: String -> Maybe Integer
parseGuess "0" = Just 0
parseGuess "1" = Just 1
parseGuess "2" = Just 2
parseGuess "3" = Just 3
parseGuess "4" = Just 4
parseGuess "5" = Just 5
parseGuess _   = Nothing

handleEvenOdd :: String -> Maybe EvenOdd
handleEvenOdd "even" = Just Even
handleEvenOdd "odd"  = Just Odd
handleEvenOdd _      = Nothing

integerToEvenOdd :: Integer -> EvenOdd
integerToEvenOdd x =
  if (x `mod` 2 == 0)
    then Even
    else Odd

scoreRound :: Integer -> Integer -> Game -> Game
scoreRound humanGuess compGuess game
  | integerToEvenOdd (humanGuess + compGuess) == gameChoice =
    Game (hScore + 1) cScore gameChoice "Good guess!"
  | otherwise = Game hScore (cScore + 1) gameChoice "Bad guess nerd!"
  where
    gameChoice = evenOdd game
    hScore = humanScore game
    cScore = computerScore game

handleGuess :: Maybe Integer -> Integer -> Game -> Either String Game
handleGuess (Just humanGuess) compGuess game =
  Right $ scoreRound humanGuess compGuess game
handleGuess Nothing _ _ = Left "You only have 5 fingers! Choose 0 to 5:"

declareWinner :: Game -> Either String Game
declareWinner Game {humanScore = 6}    = Left "You win!"
declareWinner Game {computerScore = 6} = Left "You lose! #sad"
declareWinner g                        = Right g

playGame :: StateT Game IO ()
playGame = do
  game <- get
  liftIO $ putStrLn "Guess a number between 0 and 5"
  humanGuess <- liftIO $ parseGuess <$> getLine
  computerGuess <- liftIO $ randomRIO (0 :: Integer, 5)
  case handleGuess humanGuess computerGuess game of
    Right newGame -> do
      liftIO $ putStrLn $ message newGame
      put newGame
    Left m -> do
      liftIO $ putStrLn m
      playGame
  newGame <- get
  case declareWinner newGame of
    Left msg -> liftIO $ putStrLn msg
    Right g -> do
      liftIO $ print g
      playGame

main :: IO ()
main = do
  putStrLn "Choose 'even' or 'odd'"
  evenOdd' <- handleEvenOdd <$> getLine
  case evenOdd' of
    Just eo -> do
      results <- execStateT playGame $ Game 0 0 eo ""
      print results
    Nothing -> do
      putStrLn "Must chose either 'even' or 'odd'"
      main
