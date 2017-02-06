sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

-- for 2.5.2 and 2.5.3
circleArea :: (Floating a) => a -> a
circleArea diameter = pi * (diameter * diameter)
