{-# LANGUAGE OverloadedStrings #-}

module Backtrack where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AttoBS
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => AttoBS.Parser a -> ByteString -> IO ()
attoP p i = print $ AttoBS.parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main'' :: IO ()
main'' = do
    print "--> Trifecta <--"
    trifP nobackParse "13"
    trifP tryParse "13"

    print "--> Parsec <--"
    parsecP nobackParse "13"
    parsecP tryParse "13"

    print "--> AttoP <--"
    attoP nobackParse "13"
    attoP tryParse "13"