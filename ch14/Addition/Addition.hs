module Addition where

import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4


rSum :: (Eq a, Num a) => a -> a -> a
rSum x y = rSumSub 0 x y

rSumSub :: (Eq a, Num a) => a -> a -> a -> a
rSumSub total x y
  | x == 0    = total
  | otherwise = rSumSub (y + total) (x - 1) y

rSumTest :: IO ()
rSumTest = hspec $ do
  describe "rSum" $ do
    it "4 * 5 should be 20" $ do
      rSum 4 5 `shouldBe` 20
    it "0 * 2 should be 0" $ do
      rSum 0 2 `shouldBe` 0
    it "12 and 3 should be 36" $ do
      rSum 12 3 `shouldBe` 36