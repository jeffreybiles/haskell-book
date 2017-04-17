module Main where

import WordNumberTest (wordNumberTest)
import UsingQuickCheck (quickCheckTests)

main :: IO ()
main = do
  wordNumberTest
  quickCheckTests
