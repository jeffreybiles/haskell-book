module Main where

import WordNumberTest (wordNumberTest)
import UsingQuickCheck (quickCheckTests)
import HangmanTest (hangmanTest)
import CiphersTest (ciphersTest)

main :: IO ()
main = do
  ciphersTest
  wordNumberTest
  quickCheckTests
  -- hangmanTest -- can't import Puzzle correctly...
