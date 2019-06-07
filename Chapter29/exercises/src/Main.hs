module Main where

import           Data.Char          (ord)
import           Data.List

import           System.Environment
import           System.IO

cipher :: Int -> String -> String
cipher _ [] = []
cipher shift (' ':xs) = " " ++ cipher shift xs
cipher shift (x:xs)
  | shift == 0 = [x]
  | x == ' ' = " " ++ cipher shift xs
  | otherwise = (last . take shift $ chars) : cipher shift xs
  where
    chars = [x .. 'z'] ++ cycle ['a' .. 'z']

uncipher :: Int -> String -> String
uncipher _ [] = []
uncipher shift (' ':xs) = " " ++ uncipher shift xs
uncipher shift (x:xs)
  | shift == 0 = [x]
  | x == ' ' = " " ++ uncipher shift xs
  | otherwise = (last . take shift $ start ++ finish) : uncipher shift xs
  where
    start = reverse ['a' .. x]
    finish = cycle . reverse $ ['a' .. 'z']

vigenere :: (Int -> String -> String) -> String -> String -> String
vigenere _ _ [] = []
vigenere f a b = go a b 0
  where
    shiftSize xs n
      | d >= 0 = d
      | otherwise = abs d + 1
      where
        d = ord (head xs) - ord (xs !! n)
    go xs ys n
      | null xs = []
      | head xs == ' ' = " " ++ go (tail xs) ys n
      | n == length ys - 1 = f (shiftSize ys n) [head xs] ++ go (tail xs) ys 0
      | otherwise = f (shiftSize ys n) [head xs] ++ go (tail xs) ys (n + 1)

vigenereEncode :: String -> String -> String
vigenereEncode = vigenere cipher

vigenereDecode :: String -> String -> String
vigenereDecode = vigenere uncipher

usage :: String
usage = "Usage: stack run -- (-d | -e) < input_file > output_file"

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then print usage
    else pure ()
  string <- hGetContents stdin
  case args of
    [option] ->
      case option of
        "-d" -> hPutStr stdout $ vigenereDecode string key
        "-e" -> hPutStr stdout $ vigenereEncode string key
        _    -> print usage
  where
    key = "world"
