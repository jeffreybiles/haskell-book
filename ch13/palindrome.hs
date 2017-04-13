import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

palindrome :: IO () 
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

isPalindrome :: String -> Bool
isPalindrome string = parsedString == reverse parsedString
  where parsedString = map toLower $ filter isLetter string