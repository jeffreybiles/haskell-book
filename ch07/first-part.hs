bindExp :: Integer -> String
bindExp x = let y = 5; z = y + x in "the integer was: "
            ++ show x ++ " and y was: "
            ++ show y ++ " and z was: "
            ++ show z

addOneIfOdd n = case odd n of
  True -> (\n -> n + 1) n
  False -> n

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

-- exercises for 7.4
-- 1a: (a, b) -> a
-- 1b: [Char]
-- 1c: k1 and k3
-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- exercises for 7.5
-- 1
functionC x y =
  case x > y of
    True -> x
    False -> y
-- 2
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n
-- 3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- exercises for 7.6
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
