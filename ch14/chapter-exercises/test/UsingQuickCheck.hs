module UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)

quickCheckTests :: IO ()
quickCheckTests = do
  quickCheck prop_halfIsIdentity --1
  quickCheck prop_listOrdered --2
  quickCheck plusAssociative --3
  quickCheck plusCommutative --3
  quickCheck multAssociative --4
  quickCheck multCommutative --4
  quickCheck quotRemCheck --5
  quickCheck divModCheck --5
  -- quickCheck powAssociative --6
  -- quickCheck powCommutative --6
  quickCheck listReverse --7

half x = x / 2
halfIdentity = (*2) . half

prop_halfIsIdentity :: Double -> Bool
prop_halfIsIdentity x = (halfIdentity x) == x

listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered l = listOrdered $ sort l

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z
multCommutative :: Int -> Int -> Bool
multCommutative x y =
  x * y == y * x

quotRemCheck :: Int -> Int -> Bool
quotRemCheck _ 0 = True
quotRemCheck x y =
  (quot x y)*y + (rem x y) == x
divModCheck :: Int -> Int -> Bool
divModCheck _ 0 = True
divModCheck x y =
  (div x y)*y + (mod x y) == x

powAssociative :: Int -> Int -> Int -> Bool
powAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z
powCommutative :: Int -> Int -> Bool
powCommutative x y =
  x ^ y == y ^ x

listReverse :: [Int] -> Bool
listReverse l = reverse (reverse l) == l
