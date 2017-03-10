-- 1
myWords :: String -> [String]
myWords string = mySplit string ' '



-- 2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines string = mySplit string '\n'

-- 3
mySplit :: String -> Char -> [String]
mySplit string char
  | length string == 0 = []
  | otherwise = word : (mySplit rest char)
        where word = takeWhile (/=char) string
              rest = dropWhile (==char) (dropWhile (/=char) string)
