module Main where

import Test.Hspec

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multByAdd :: (Integral a) => a -> a -> a
multByAdd x 0 = 0
multByAdd x n = x + multByAdd x (n - 1)

main :: IO ()
main = hspec $ describe "Addition" $ do
  it "1 + 1 greater than 1" $
    (1 + 1) > 1 `shouldBe` True
  it "2 + 2 is equal to 4" $
    2 + 2 `shouldBe` 4
  it "15 divided by 3 is 5" $
    15 `dividedBy` 3 `shouldBe` (5, 0)
  it "22 divided by 5 is 4 remainder 2" $
    22 `dividedBy` 5 `shouldBe` (4, 2)
  it "2 times 2 is 4" $
    2 `multByAdd` 2 `shouldBe` 4
  it "4 times 4 is 16" $
    4 `multByAdd` 4 `shouldBe` 16
  it "4 times 0 is 0" $
    4 `multByAdd` 0 `shouldBe` 0
  it "0 times 4 is 0" $
    0 `multByAdd` 4 `shouldBe` 0
