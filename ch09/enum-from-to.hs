eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x == y = [x]
  | x < y = [x, y]
  | otherwise = []

-- WTF is "Ordering"?  Anyways, this works
eftOrd :: (Enum a, Ord a) => a -> a -> [a]
eftOrd x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftOrd (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y = eftOrd x y

eftChar :: Char -> Char -> [Char]
eftChar x y = eftOrd x y
