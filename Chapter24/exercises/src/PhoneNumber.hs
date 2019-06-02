{-# LANGUAGE LambdaCase #-}
module PhoneNumber where

import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Integer
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber =
    PhoneNumber
    { numberingPlanArea :: NumberingPlanArea
    , exchange :: Exchange
    , lineNumber :: LineNumber
    } deriving (Eq, Show, Ord)

-- 123-456-7890
-- 1234567890
-- (123) 456 7890
-- 1-123-456-7890

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber = do
    numberingPlanArea <- countryCodeNPA <|> npa <|> parensNPA
    c <- optional dashOrSpace
    exchange <- nDigits 3
    c' <- optional dashOrSpace
    lineNumber <- nDigits 4
    case (c, c') of
        (Just '-', Just ' ') -> fail "Invalid Phone Number"
        _ -> return $ PhoneNumber numberingPlanArea exchange lineNumber
    where
        nDigits n = read <$> (count n $ oneOf "1234567890")
        npa = nDigits 3
        countryCodeNPA = (optional $ (try $ char '+' *> integer *> char '-')) *> npa
        parensNPA = char '(' *> npa <* char ')'
        prefix = countryCodeNPA <|> npa <|> parensNPA
        dashOrSpace = char '-' <|> char ' '

parsePhoneNumber' :: Parser PhoneNumber 
parsePhoneNumber' = 
        try (PhoneNumber <$> numberingPlanArea <*> exchange <*> lineNumber)
    <|> try (PhoneNumber <$> numberingPlanArea <*> (char ' ' *> exchange) <*> (char ' ' *> lineNumber))
    <|> try (PhoneNumber <$> numberingPlanArea <*> (char '-' *> exchange) <*> (char '-' *> lineNumber))
    <|> try (PhoneNumber <$> numberingPlanArea <*> (char ' ' *> exchange) <*> (char '-' *> lineNumber))
        where
            nDigits n = read <$> (count n $ oneOf "1234567890")
            threeDigits = nDigits 3
            countryCodePattern = (optional $ (try $ char '+' *> integer *> char '-')) *> threeDigits
            parensPattern = char '(' *> threeDigits <* char ')'
            numberingPlanArea = countryCodePattern <|> threeDigits <|> parensPattern
            exchange = nDigits 3
            lineNumber = nDigits 4

parsePhoneNumber'' :: Parser PhoneNumber 
parsePhoneNumber'' = (PhoneNumber <$> numberingPlanArea) `applyTuple` tupled
    where
        nDigits n = read <$> (count n $ oneOf "1234567890")
        threeDigits = nDigits 3
        countryCodePattern = (optional $ (try $ char '+' *> integer *> char '-')) *> threeDigits
        parensPattern = char '(' *> threeDigits <* char ')'
        numberingPlanArea = countryCodePattern <|> parensPattern
        dashOrSpace = char '-' <|> char ' '
        tupled :: Parser (Exchange, LineNumber)
        tupled = do
            c <- optional dashOrSpace
            ex <- nDigits 3
            c' <- optional dashOrSpace
            ln <- nDigits 4
            case (c, c') of
                (Just '-', Just ' ') -> fail "Invalid Phone Number"
                _                    -> return (ex, ln)
        applyTuple f a = uncurry <$> f <*> a
