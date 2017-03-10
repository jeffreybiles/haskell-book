import Data.Char

onlyCaps :: String -> String
onlyCaps string = filter isUpper string

toCaps :: String -> String
toCaps "" = ""
toCaps (x:xs) = (toUpper x) : toCaps(xs)

capitalizeFirst :: String -> Char
capitalizeFirst (x:xs) = toUpper x

capitalizeFirst' :: String -> Char
capitalizeFirst' = toUpper . head
