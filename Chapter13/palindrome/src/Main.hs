module Main where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)

lowercaseString :: String -> String
lowercaseString = map toLower

removeNonAlpha :: String -> String
removeNonAlpha = filter isAlpha

palindrome :: IO ()
palindrome = forever $ do
  putStrLn "Check string is a palindrome:"
  line <- getLine
  let sanitizedLine = removeNonAlpha . lowercaseString $ line
  if sanitizedLine == reverse sanitizedLine then
    putStrLn $ line ++ " is a palindrome!"
  else do
    putStrLn $ line ++ " is NOT a palindrome!"
    exitSuccess

main :: IO ()
main = palindrome
