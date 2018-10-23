module Main where

-- import Test.Hspec
import Test.QuickCheck

-- main :: IO ()
-- main = hspec $
--   it "x + 1 is always greater than x" $
--     property $ \x -> x + 1 > (x :: Integer)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

main :: IO ()
main = quickCheck prop_additionGreater
