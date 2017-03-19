import Data.Char
import Data.List
import Data.List.Split
-- 1: a
-- 2: c
-- 3: b
-- 4: c

-- see vigenere-cipher file

-- as-patterns
-- 1 -- why would this need at-patterns?
isSubsequence :: (Eq a) => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence (x:xs) full = x `elem` full && isSubsequenceOf xs full
-- 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords string = map (\word@(x:xs) -> (word, toUpper x : xs)) $ words string

-- language exercises
-- 1
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord _ = ""

-- 2
-- This feels wrong
capitalizeParagraph :: String -> String
capitalizeParagraph para = intercalate ". " capitalizedSentences
  where sentences = splitOn ". " para
        capitalizedSentences = map capitalizeWord sentences

-- I hate the phone exercise.  Poorly explained.  Massive difficulty spike.
-- And WTF is "cellPhonesDead" supposed to do, anyways?  Is this some gen-Y in-joke?

-- Hutton's Razor
-- 1
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)

-- 2
printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)
