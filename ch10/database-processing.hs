import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
      , DbNumber 9001
      , DbString "Hello, world!"
      , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
      , DbNumber 22
  ]

-- this is the best I can get.  The rest of them will be variations on this pattern.
-- Surely there's a better pattern.  Some function I can feed into filter to filter by type.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate x):xs) = x : filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber x):xs) = x : filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

-- unsafe, but I couldn't think of a good base case for time
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr1 max (filterDbDate db)

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDbNumber db)

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ length $ filterDbNumber db)
