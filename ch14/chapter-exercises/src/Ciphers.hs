module Ciphers where

import Data.Char

firstOrd = ord 'a'
lastOrd = firstOrd + 26

caesar :: String -> Int -> String
caesar string shift = map chr $ map (\x -> if x > lastOrd then x - 26 else x) $ map (+shift) $ map ord string


unCaesar :: String -> Int -> String
unCaesar string shift = map chr $ map (\x -> if x < firstOrd then x + 26 else x) $ map (\x -> x - shift) $ map ord string
