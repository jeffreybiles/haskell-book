{-# LANGUAGE NoMonomorphismRestriction #-}

fiftyfour = (*9) 6
b = head [(0, "doge"),(1, "kitteh")]
c = head [(0 :: Integer, "doge"),(1, "kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
f = (length [1, 2, 3, 4, 5]) > (length "TACOCAT")

-- Writing a type signature

-- 1
-- functionH :: [a] -> a
functionH (x:_) = x
-- 2
-- functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False
-- 3
-- functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
-- 1
i :: a -> a
i x = x
-- 2
c2 :: a -> b -> a
c2 x _ = x
-- 3
-- YES
-- 4
c' :: a -> b -> b
c' _ y = y
-- 5
r :: [a] -> [a]
r x = tail x
-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)
-- 7
a2 :: (a -> c) -> a -> a
a2 _ x = x
-- 8
a' :: (a -> b) -> a -> b
a' aToB a = aToB a
