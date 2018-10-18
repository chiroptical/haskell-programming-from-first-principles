module Main where

import Cipher (cipher)
import Control.Monad (forever)
import Text.Read (readMaybe)

caesarCipher :: IO ()
caesarCipher = forever $ do
  putStrLn "What string would you like to encode?"
  line <- getLine
  putStrLn "What shift would you like to apply?"
  shift <- getLine
  case readMaybe shift :: Maybe Int of
    Just x -> putStrLn $ cipher x line
    _ -> putStrLn "Shift isn't an integer?"

main :: IO ()
main = caesarCipher
