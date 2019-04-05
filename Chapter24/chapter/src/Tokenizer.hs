{-# LANGUAGE OverloadedStrings #-}

module Tokenizer where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Data.Attoparsec.Text (parseOnly)
import Data.String (IsString)

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    num <- decimal
    _ <- char '/'
    den <- decimal
    case den of
        0 -> fail "Denominator cannot be zero!"
        _ -> return (num % den)

main' :: IO ()
main' = do
    let attoP = parseOnly parseFraction
    print $ attoP badFraction
    print $ attoP alsoBad
    print $ attoP shouldWork
    print $ attoP shouldAlsoWork

    let p i = parseString parseFraction mempty i
    print $ p badFraction
    print $ p alsoBad
    print $ p shouldWork
    print $ p shouldAlsoWork