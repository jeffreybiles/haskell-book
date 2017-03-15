myOr :: [Bool] -> Bool
myOr bools = foldr (||) False bools

myAny :: (a -> Bool) -> [a] -> Bool
myAny func items = foldr ((||) . func) False items

myElem :: Eq a => a -> [a] -> Bool
myElem target list = foldr ((||) . (==target)) False list

myReverse :: [a] -> [a]
myReverse string = foldl (flip (:)) [] string

myMap :: (a -> b) -> [a] -> [b]
myMap func list = foldr ((:) . func) [] list

-- how would we pointfree this?
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter func list = foldr (\x acc -> if (func x) then x:acc else acc) [] list

squish :: [[a]] -> [a]
squish arrays = foldr (++) [] arrays

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap func array = foldr ((++) . func) [] array

squishAgain :: [[a]] -> [a]
squishAgain arrays = squishMap id arrays

-- How to make pointfree?
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comparison array = foldr1 (\x acc -> if (comparison x acc) == GT then x else acc) array

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comparison array = foldr1 (\x acc -> if (comparison x acc) == LT then x else acc) array
