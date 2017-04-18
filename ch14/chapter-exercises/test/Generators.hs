module Generators where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)


data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = oneof [return Fulse, return Frue]

weightedFoolGen :: Gen Fool
weightedFoolGen = frequency [(2, return Fulse),
                             (1, return Frue)]
