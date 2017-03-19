module Cipher where

import Data.Char

shift :: Char -> Char -> Char
shift shiftee letter = chr shifted
    where shiftAmount = (ord letter) - (ord 'a')
          naiveShifted = (ord shiftee) + shiftAmount
          shouldLoop = naiveShifted > (ord 'z')
          shifted = if shouldLoop then naiveShifted - 26 else naiveShifted

vigenere :: String -> String -> String
vigenere [] _ = []
vigenere (' ':xs) shiftString = ' ' : vigenere xs shiftString
vigenere (x:xs) (y:ys) = (shift x y) : vigenere xs (ys ++ [y])
