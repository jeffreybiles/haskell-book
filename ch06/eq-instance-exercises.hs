data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- -- 1
data TisAnInteger =
       TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

-- 2
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') =
      a == a' && b == b'

-- 3
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b

-- 4
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d

-- 5
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple c d) (Tuple c' d') = c == c' && d' == d'

-- 6

data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne b) (ThisOne b') = b == b'
  (==) (ThatOne b) (ThatOne b') = b == b'
  (==) (ThisOne b) (ThatOne b') = b == b'
  (==) (ThatOne b) (ThisOne b') = b == b'

-- 7

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello c) (Hello c') = c == c'
  (==) (Goodbye c) (Goodbye c') = c == c'
