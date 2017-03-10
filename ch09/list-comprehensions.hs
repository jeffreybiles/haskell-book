import Data.Bool

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- square cube

[(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- filter exercise 3

myFilter str = filter (\x -> not (elem x ["a", "an", "the"])) (words str)
