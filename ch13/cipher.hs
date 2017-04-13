module Cipher where

import Data.Char

firstOrd = ord 'a'
lastOrd = firstOrd + 26

ceasar :: String -> Int -> String
ceasar string shift = map chr $ map (\x -> if x > lastOrd then x - 26 else x) $ map (+shift) $ map ord string


unCeasar :: String -> Int -> String
unCeasar string shift = map chr $ map (\x -> if x < firstOrd then x + 26 else x) $ map (\x -> x - shift) $ map ord string

main = do
  putStr "Enter a phrase: "
  phrase <- getLine
  putStr "Enter integer: "
  shift <- getLine
  putStrLn $ ceasar phrase (read shift :: Int)