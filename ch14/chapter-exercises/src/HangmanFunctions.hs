module HangmanFunctions where

data Puzzle = Puzzle String [Maybe Char] [Char]

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          let zd = (zipper c)
          in zipWith zd word filledInSoFar
