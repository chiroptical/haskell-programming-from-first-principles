module Cipher where

import Data.Char (ord)

cipher :: Int -> String -> String
cipher shift [] = []
cipher shift (' ':xs) = " " ++ cipher shift xs
cipher shift (x:xs)
    | shift == 0 = [x]
    | x == ' ' = " " ++ cipher shift xs
    | otherwise = [last . take shift $ chars] ++ cipher shift xs
        where chars = [x..'z'] ++ cycle ['a'..'z']

uncipher :: Int -> String -> String
uncipher shift [] = []
uncipher shift (' ':xs) = " " ++ uncipher shift xs
uncipher shift (x:xs)
    | shift == 0 = [x]
    | x == ' ' = " " ++ uncipher shift xs
    | otherwise = [last . take shift $ start ++ finish] ++ uncipher shift xs
        where start  = reverse ['a'..x]
              finish = cycle . reverse  $ ['a'..'z']


vigenere :: (Int -> String -> String) -> String -> String -> String
vigenere _ _ [] = []
vigenere f a b = go a b 0
    where shiftSize xs n
            | d >= 0 = d
            | otherwise = (abs d) + 1
                where d = ord (head xs) - ord (xs !! n)
          go xs ys n
            | xs == [] = []
            | head xs == ' ' = " " ++ go (tail xs) ys n
            | n == length ys - 1 = f (shiftSize ys n) [head xs] ++ go (tail xs) ys 0
            | otherwise = f (shiftSize ys n) [head xs] ++ go (tail xs) ys (n + 1)

vigenereEncode = vigenere cipher
vigenereDecode = vigenere uncipher
