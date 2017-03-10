--

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny func (x:xs) = if (func x) then True else myAny func xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem item (x:xs) = if (x == item) then True else myElem item xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse string = (last string) : (myReverse $ init string)

squish :: [[a]] -> [a]
squish [] = []
squish (array:arrays) = array ++ (squish arrays)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap func (array:arrays) = (func array) ++ squishMap func arrays

squishAgain :: [[a]] -> [a]
squishAgain arrays = squishMap id arrays

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy func array
  | length array == 1 = head array
  | otherwise = if ((func (head array) result) == GT) then (head array) else result
      where result = myMaximumBy (func) (tail array)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy func array
  | length array == 1 = head array
  | otherwise = if ((func (head array) result) == LT) then (head array) else result
      where result = myMaximumBy (func) (tail array)
