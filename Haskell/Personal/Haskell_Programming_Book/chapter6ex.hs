import Data.List

-- use newtype when only one value constr with one param, 
-- decreases laziness
newtype TisAnInteger = TisAn Integer
data TwoIntegers = Two Integer Integer
data StringOrInt = TisAnInt Int | TisAString String
data Pair a = Pair a a 
data Tuple a b = Tuple a b
data Which a = ThisOne a | ThatOne a
data EitherOr a b = Hello a | Goodbye b
data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
                 deriving (Eq, Show)

-- So friday is the best day
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ 

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = a == c && b == d

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

instance Eq a => Eq (Pair a) where
  (==) (Pair c d) (Pair v w) = c == v && w == d

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v w) (Tuple c d) = v == c && w == d

instance Eq a => Eq (Which a) where
  (==) (ThisOne b) (ThisOne c) = b == c
  (==) (ThatOne b) (ThatOne c) = b == c
  (==) _ _ = False

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False


newtype Person = Person Bool

instance Show Person where
  show (Person True) = "True"
  show (Person False) = "False"

data Mood = Blah | Woot deriving (Show, Eq, Ord)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

-- typechecks but s1 don't work 
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

newtype Rocks = Rocks String deriving (Eq, Show)
newtype Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- phew = Papu "chases" True -- don't work, not valid value constructors
truth = Papu (Rocks "chomskydoz") (Yeah True) -- works

equalityForAll :: Papu -> Papu -> Bool -- works
equalityForAll p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool -- don't work, must impl Ord
-- comparePapus p p' = p > p'

-- i :: a doesn't work, doesn't know it's Num
i :: Num a => a
i = 1

-- f :: Float works, the one below doesn't
-- f :: Num a => a
-- f :: Float
-- f = 1.0

-- works, RealFrac is instance of Float, equivalent to f :: Float
f :: RealFrac a => a
f = 1.0

-- not all data types implement Ord, not exhaustive, better be freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- jung :: Ord a => [a] -> a - more generic
jung :: [Int] -> Int
jung = minimum

-- young :: [Char] -> Char - same logic as prev example
young :: Ord a => [a] -> a
young = minimum

-- chapter 6 page 212
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x = f
