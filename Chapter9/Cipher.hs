module Cipher where

cipher :: Int -> String -> String
cipher shift [] = []
cipher shift (' ':xs) = " " ++ cipher shift xs
cipher shift (x:xs)
    | x == ' ' = " " ++ cipher shift xs
    | otherwise = [last . take shift $ chars] ++ cipher shift xs
        where chars = [x..'z'] ++ cycle ['a'..'z']

uncipher :: Int -> String -> String
uncipher shift [] = []
uncipher shift (' ':xs) = " " ++ uncipher shift xs
uncipher shift (x:xs)
    | x == ' ' = " " ++ uncipher shift xs
    | otherwise = [last . take shift $ start ++ finish] ++ uncipher shift xs
        where start  = reverse ['a'..x]
              finish = cycle . reverse  $ ['a'..'z']
