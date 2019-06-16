module StoppingTheParty where

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad      (forever)
import           System.Random

import           WhySomeException

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1 .. 9]
    then throwIO $ MyException DivideByZero
    else throwIO $ MyException StackOverflow

runStoppingTheParty :: IO ()
runStoppingTheParty =
  forever $ do
    let tryS :: IO () -> IO (Either MyException ())
        tryS = try
    _ <- tryS randomException
    putStrLn "Live to loop another day!"
    threadDelay 1000000
