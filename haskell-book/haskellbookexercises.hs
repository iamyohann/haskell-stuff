import Data.List (intersperse)
import Data.Typeable
import qualified Data.Char as Char

foo :: Integer
foo = let
        a = 10
        b = 20
      in a + b

waxOn :: Integer -> Integer
waxOn x = x * m where m = 5

{- 

a) 
-- Given
"Curry is awesome" 
-- Return
"Curry is awesome!"

b) 
-- Given
"Curry is awesome!" 
-- Return
"y"

c) 
-- Given
"Curry is awesome!" 
-- Return "awesome!"


-}

fnA :: String -> String
fnA s = s ++ "!"

fnB :: String -> String
fnB s = take 1 $ drop 4 s

myNot :: Bool -> Bool
myNot True = False
myNot _ = True


data Mood = Blah | Woot deriving (Show)

reverse' :: [a] -> [a]
reverse' s = case s of
                [] -> []
                [x] -> [x]
                (x: y: ys) -> reverse ys ++ [y, x]

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x 
  | x >= 0 = x
  | otherwise = x * (-1)


head2 :: [a] -> a
head2 (_:y:_) = y
head2 [] = error "Empty list"
head2 [_] = error "Too small"

add2Nums :: (Num a) => a -> a -> a
add2Nums a b = a + b 


compareLess :: Ord a => a -> a -> Bool
compareLess a b = a < b

anonCurry :: Num a => a -> a -> a -> a
anonCurry = \x y z -> x + y + z

f :: a -> a -> a -> a
f a b c = a

myConcat x = x ++ " abc"

functionH :: [x] -> x
functionH (x:_) = x


functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (_, y) = y


co :: (b -> c) -> (a -> b) -> (a -> c)
co b2c a2b = b2c . a2b

data Something = Lol
instance Show Something where
  show _ = "Foo"

data Year2 = Year2 Integer deriving (Eq, Show)

class Intable a where
  fromIntlol :: Integer -> a
  toIntlol :: a -> Integer
  (??) :: a -> a -> a

instance Intable Year2 where
  fromIntlol n = Year2 n
  toIntlol (Year2 n) = n
  (??) (Year2 a) (Year2 b) = Year2 (a + b)

sumYear :: [Year2] -> Year2
sumYear ys = fromIntlol $ sum $ map toIntlol ys

data Trivial a = Trivial a

instance Eq (Trivial a) where
  (==) _ _ = True

type Day = Integer
type Month = Integer
type Year = Integer

data Date = Date Year Month Day deriving (Eq, Show, Ord)

class DateStuff a where
  maxD :: a
  minD :: a
  isValid :: a -> Bool
  
instance DateStuff Date where
  maxD = Date 2099 12 31
  minD = Date 0 1 1
  isValid d = minD < d && d < maxD

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity b) = a == b

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = a == c && b == d

data StringOrInt = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = a == c && b == d

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False

data Rocks =
  Rocks String deriving (Eq, Show)
data Yeah =
  Yeah Bool deriving (Eq, Show)
data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

f1 :: Fractional a => a 
f1 = 1.0

-- let binding
incText :: (Num a, Show a) => a -> String
incText x = let y = x + 1 in "The increment of " ++ show x ++ " is " ++ show y

multipleLet :: Integer -> Integer
multipleLet x = let z = x + y; y = x + 1 in x + y + z

newtype Username = Username String deriving (Show, Eq)
newtype AccountNumber = AccountNumber Integer deriving (Show, Eq)

data User = UnregisteredUser | RegisteredUser Username AccountNumber deriving (Show, Eq)

printUser :: User -> String
printUser u = case u of
  UnregisteredUser -> "Foo"
  RegisteredUser (Username us) (AccountNumber a) -> "User: " ++ us ++ ", Account: " ++ show a

k :: (a, b) -> a
k (x, y) = x

reverse2' :: [a] -> [a]
reverse2' a = case a of
  [] -> []
  [x] -> [x]
  (x:y:xs) -> (reverse2' xs) ++ [y, x]

isZero :: (Num a, Eq a) => a -> Bool
isZero n
  | n == 0 = True
  | otherwise = False

factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n < 0 = error "Must be greater than 0"
  | n > 0 = factorial (n-1) * n

fibonacci :: Integer -> Maybe Integer
fibonacci n
  | n == 0 = Just 0
  | n == 1 = Just 1
  | n > 1 = fmap sum $ sequence [fibonacci (n - 1), fibonacci (n-2)]
  | otherwise = Nothing

fibonacciseries :: Integer -> Integer -> [Maybe Integer]
fibonacciseries a b = fmap fibonacci $ [a..b]

type Quotient = Integer
type Remainder = Integer
type Numerator = Integer
type Denominator = Integer

divideBy :: Numerator -> Denominator -> (Quotient, Remainder)
divideBy num denom = go num denom 0
  where go n d q
          | n < d = (q, n)
          | otherwise = go (n - d) d (q + 1)

sumN :: (Eq a, Num a, Ord a) => a -> Maybe a
sumN n
  | n == 0 = Just 0
  | n > 0 = sum <$> sequence [Just n, sumN (n - 1)]
  | otherwise = Nothing

  

data DividedResult = Result (Integer, Integer) | DivideByZeroError

-- betterDivideBy :: Integer -> Integer -> DividedResult
-- betterDivideBy num denom = go num denom 0
--   where go n d q
--           | d == 0 = DivideByZeroError
--           | n < d = Result (q, n)
--           | (n > 0 && d > 0) || (n < 0 && d < 0) = go ((abs n) - (abs d)) (abs d) (q + 1)
--           | otherwise = go (n + d) (d) (q - 1)


mc91 :: Integer -> Integer
mc91 n
  | n > 100 = (n - 10)
  | otherwise = mc91 $ mc91 $ n + 11

digitToWord :: Integer -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "Not supported"

numberToDigits :: Integer -> [Integer]
numberToDigits n 
  | n >= 0 = case div n 10 of
              0 -> [mod n 10]
              _ -> numberToDigits (div n 10) ++ [mod n 10]
  | otherwise = error "Not supported"

wordNumber :: Integer -> String
wordNumber n = concat $ intersperse "-" (map digitToWord $ numberToDigits n)

data Foo = Foo Integer Integer | FooError String deriving (Show)

makeFoo :: Integer -> Integer -> Foo
makeFoo one two
  | one == 1 = Foo 1 two
  | otherwise = FooError "Blah"


hasError :: Foo -> Bool
hasError (FooError _) = True
hasError _ = False

showOne :: Foo -> Integer
showOne (Foo o _) = o
showOne _ = error "blah"

eftBool :: Bool -> Bool -> [Bool]
eftBool a b
  | a == b = [a]
  | a < b = a : eftBool (succ a) b
  | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a == b = [a]
  | a < b = a : eftInt (succ a) b
  | otherwise = []


enumFromTo' :: (Enum a,Ord a, Eq a) => a -> a -> [a]
enumFromTo' a b
  | a == b = [a]
  | a < b = a : enumFromTo (succ a) b
  | otherwise = []



myWords :: String -> [String]
myWords [] = []
myWords s = (takeWhile (/=' ') s) : myWords (dropWhile (==' ') $ dropWhile (/=' ') s)

sepStrAt' :: Char -> String -> [String]
sepStrAt' _ [] = []
sepStrAt' sep s = (takeWhile (/=sep) s) : sepStrAt' sep (dropWhile (==sep) $ dropWhile (/=sep) s)

lines' :: String -> [String]
lines' s = sepStrAt' '\n' s

squares :: Integer -> [Integer]
squares n = [x ^ 2 | x <- [1..n]]

evenSquares :: Integer -> [Integer]
evenSquares n = [x ^ 2 | x <- [1..n], mod x 2 == 0]

mySquareCubes n = [(x^2, x^3) | x <- [1..n], x < 50]


multiplesOfThree x = filter ((== 0) . (flip mod) 3) x

numberOfMultiplesOfThree = length . multiplesOfThree

filterUpperCase :: String -> String
filterUpperCase = filter Char.isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (Char.toUpper x):xs

capitalizeFirstLetter :: String -> Char
capitalizeFirstLetter = (Char.toUpper . head)


myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny fn (x:xs) = if (fn x) == True then True else myAny fn xs


myElem :: (Eq a) => a -> [a] -> Bool
myElem needle l = myAny (==needle) l

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:y:xs) = myReverse xs ++ [y, x]

squish :: [[a]] -> [a]
squish [] = []
squish [x] = x
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap fn [x] = fn x
squishMap fn (x:xs) = (fn x) ++ (squishMap fn xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

