module UsingQuickCheck where

import Test.QuickCheck
import Test.QuickCheck.Function (apply, Fun(..))
import Data.List (sort)
import Data.Char (toUpper)

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
  quickCheck applicationOperator --8
  -- quickCheck foldCons --9
  quickCheck foldAppend --9
  -- quickCheck lengthFunc --10
  quickCheck roundTrip
  quickCheck capitalizeIdempotence

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

applicationOperator :: Fun Int Int -> Int -> Bool
applicationOperator (Fun _ f) a = (f a) == (f $ a)

foldCons :: String -> String -> Bool
foldCons arr arr2 = (foldr (:) arr arr2) == ((++) arr arr2)

foldAppend :: [String] -> Bool
foldAppend arr = (foldr (++) [] arr) == (concat arr)

lengthFunc :: Int -> String -> Bool
lengthFunc n xs = length (take n xs) == n

roundTrip :: Int -> Bool
roundTrip x = (read (show x)) == x

twice f = f . f
fourTimes = twice . twice
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeIdempotence :: String -> Bool
capitalizeIdempotence x = (capitalizeWord x == twice capitalizeWord x) && (twice capitalizeWord x == fourTimes capitalizeWord x)
