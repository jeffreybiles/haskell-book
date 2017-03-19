{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- Cardinality
-- 1: 1
-- 2: 3
-- 3: 65536
-- 4: Int is bounded but large.  Integer has cardinality of infinity
-- 5: 2^8 = 256

-- For example
data Example = MakeExample deriving Show
-- 1: It errors: says Data constructor Example not in scope.  If we type MakeExample then it gives us a type of Example
-- 2: It works
-- 3: Same as #1 for Example.  For MakeExample gives is MakeExample :: Int -> Example


class TooMany a where tooMany :: a -> Bool
instance TooMany Int where tooMany n = n > 42

instance TooMany (Int, String) where tooMany (n, s) = n > 42
instance TooMany (Int, Int) where tooMany (n1, n2) = (n1 + n2) > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-- WTF: Why is it only deriving for the first instance of TooMany?
-- Works:
-- tooMany (Goats 42)
-- Doesn't work:
-- tooMany ((Goats 42), "Hi")
-- tooMany ((Goats 42), (Goats 1))

-- Pity the Bool
-- 1: 4 (2 + 2)
-- 2: 258 (256 + 2)

-- How does your garden grow?
-- 1
type Gardener = String
data Garden = Garden Gardener Gardenia
            | Garden Gardener Daisy
            | Garden Gardener Rose
            | Garden Gardener Lilac
            deriving Show
