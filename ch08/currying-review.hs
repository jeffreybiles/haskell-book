cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
apedCatty = cattyConny "woops"
frappe = flippy "haha"

-- Recursion #2
rSum :: (Eq a, Num a) => a -> a
rSum n = rSumSub 0 n

rSumSub :: (Eq a, Num a) => a -> a -> a
rSumSub total n
  | n == 0    = total
  | otherwise = rSumSub (n + total) (n - 1)

-- Recursion #3

rProd :: (Eq a, Num a) => a -> a
rProd n = rProdSub 1 n

rProdSub :: (Eq a, Num a) => a -> a -> a
rProdSub total n
  | n == 0    = total
  | otherwise = rProdSub (total * n) (n - 1)

-- Divided by 0

sign :: Integer -> Integer
sign n
  | n < 0     = -1
  | n == 0    = 0
  | otherwise = 1

data DividedResult =
     Result Integer
   | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num denom
  where go n d
         | d == 0 = DividedByZero
         | d < 0 && n >= 0 = Result (((dividedBy' n (abs d) 0) + 1) * (-1))
         | d < 0 && n < 0 = Result ((dividedBy' (abs n) (abs d) 0))
         | n < 0 = Result (((dividedBy' (abs n) d 0) + 1) * (-1))
         | otherwise = Result (dividedBy' n d 0)

dividedBy' n d count
     | n < d  = count
     | otherwise = dividedBy' (n - d) d (count + 1)

-- mc91

mc91 n
  | n > 100 = n - 10
  | otherwise = 91

-- numbers to words
