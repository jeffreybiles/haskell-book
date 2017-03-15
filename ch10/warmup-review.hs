stops  = "pbtdkg"
vowels = "aeiou"

multiplyAndCombineStrings string amount = foldr (\x prod -> prod ++ (take amount $ repeat x)) [] string
combineAndTake amount strings = take amount $ foldr (++) [] $ repeat $ strings

-- 1a
threeples :: [a] -> [a] -> [(a, a, a)]
threeples stps vwls = zip3 frst scnd thrd
  where totalLength = (length stps * length stps * length vwls)
        frst = multiplyAndCombineStrings stps (length stps * length vwls)
        scnd = combineAndTake totalLength $ multiplyAndCombineStrings vwls (length stps)
        thrd = combineAndTake totalLength $ multiplyAndCombineStrings stps 1

-- 1b
threepppppples :: [Char] -> [Char] -> [(Char, Char, Char)]
threepppppples stps vwls = filter (\(x, _, _) -> x == 'p') $ threeples stps vwls

-- 1c
nouns = ["cat", "dog", "shoe", "human"]
verbs = ["chases", "hugs", "codes", "transforms"]

-- 2
-- this gets the average word length in a string
seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
           (length (words x))

-- 3
avgWordLength :: String -> Double
avgWordLength x =
    (/) (fromIntegral $ sum (map length (words x)))
           (fromIntegral $ length (words x))
