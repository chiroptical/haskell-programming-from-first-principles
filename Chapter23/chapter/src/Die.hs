module Die where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne 
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie' rollDie' rollDie'

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = (,,) <$> rollDie <*> rollDie <*> rollDie

infinitySameDie :: State StdGen [Die]
infinitySameDie = repeat <$> rollDie

rollNDie :: Int -> State StdGen [Die]
rollNDie n = replicateM n rollDie

rollsToPassOrEqualTwenty :: StdGen -> Int
rollsToPassOrEqualTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

go :: Int -> Int -> [Die] -> Int -> StdGen -> (Int, [Die])
go sum count xs lim gen
    | sum >= lim = (count, xs)
    | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) (xs ++ [intToDie die]) lim nextGen
            
rollsToPassOrEqual :: Int -> StdGen -> Int
-- rollsToPassOrEqual = (fst .) . go 0 0 []
-- rollsToPassOrEqual lim = fst . go 0 0 [] lim
rollsToPassOrEqual lim gen = fst $ go 0 0 [] lim gen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged = go 0 0 []