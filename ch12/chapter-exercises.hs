-- determining kinds
-- 1: *
-- 2:
--  a: *
--  f: * -> *

-- string processing
-- 1
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe string = unwords $ map (\x -> if (notThe x == Nothing) then "a" else x) $ words string
-- 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel string = countTheBeforeVowel' $ words string

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (x:[]) = 0
countTheBeforeVowel' (x:y:xs) = isTheBeforeVowel x y + countTheBeforeVowel' (y : xs)

isTheBeforeVowel :: String -> String -> Integer
isTheBeforeVowel maybeThe next
    | maybeThe == "the" && (head next) `elem` "aeiou" = 1
    | otherwise                                       = 0
-- 3
countVowels :: String -> Integer
countVowels string = toInteger $ length $ filter (\x -> x `elem` "aeiou") string
-- validating word
newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord string
  | numVowels > numConsonants = Nothing
  | otherwise                 = Just $ Word' string
  where numVowels = countVowels string
        numConsonants = (toInteger $ length string) - numVowels

-- it's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ rest) = 1 + natToInteger rest

integerToNat :: Integer -> Maybe Nat
integerToNat int
  | int < 0   = Nothing
  | otherwise = Just $ integerToNat' int

integerToNat' :: Integer -> Nat
integerToNat' int
  | int == 0  = Zero
  | otherwise = Succ $ integerToNat' (int - 1)

-- Maybe
-- 1
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee deflt _ Nothing = deflt
mayybee _ func (Just a) = func a
-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe deflt mayb = mayybee deflt id mayb
-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]
-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs)  = x : catMaybes xs
-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes
    | hasNothings     = Nothing
    | otherwise       = Just $ map (\(Just x) -> x) maybes
    where hasNothings = (length $ catMaybes maybes) /= (length maybes)

-- Either
-- 1
lefts' :: [Either a b] -> [a]
lefts' eithers = foldr func [] eithers
    where func (Left a) xs  = a : xs
          func (Right b) xs = xs
-- 2
rights' :: [Either a b] -> [b]
rights' eithers = foldr func [] eithers
    where func (Left a) xs  = xs
          func (Right b) xs = b : xs
-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)
-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' func (Right b) = Just $ func b
eitherMaybe' _ _            = Nothing
-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fromLeft _  (Left a)  = fromLeft a
either' _ fromRight (Right b) = fromRight b
-- 6- WTF?  Answer will be longer, include an undefined, and be generally terrible

-- unfolds
-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate func val = val : myIterate func (func val)
-- 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr func val = a : myUnfoldr func b
    where Just(a, b) = func val
-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate func val = myUnfoldr (\x -> Just(x, func x)) val

-- binaryTrees
data BinaryTree a =
      Leaf
      | Node (BinaryTree a) a (BinaryTree a)
      deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold func input = unfold' (func input) func

unfold' :: Maybe (a, b, a) -> (a -> Maybe (a, b, a)) -> BinaryTree b
unfold' Nothing _ = Leaf
unfold' (Just(left, val, right)) func = Node (unfold func left) val (unfold func right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just(x + 1, x, x + 1)) 0
