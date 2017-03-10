module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
    | n < 10 = wordNumber n
    | otherwise = digitToWord (div n 10) ++ "-" ++ (wordNumber (mod n 10))

wordNumber :: Int -> String
wordNumber n =
  case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    0 -> "zero"
