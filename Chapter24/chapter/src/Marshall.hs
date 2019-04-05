{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshall where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ

sectionJSON :: ByteString
sectionJSON = [r|
{
    "section": {"host": "wikipedia.org"},
    "background": {"color": "white"}
}
|]

data TestData = TestData {
    section :: Host,
    background :: Color
} deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

newtype Color = Color String deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "background"
    parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where    
    parseJSON (Object v) = Host <$> v .: "host"
    parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
    parseJSON (Object v) = Color <$> v .: "color"
    parseJSON _ = fail "Expected an object for Color"
 
marshall = do
    let blah :: Maybe TestData
        blah = decode sectionJSON
    print blah 