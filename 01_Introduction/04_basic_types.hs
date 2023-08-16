import Data.Char
import Data.Complex

-- Lists
-- Lists are homogenous (e.g. the same type)
-- Lists can have any size in runtime

exampleList :: [Integer]
exampleList = [1, 2, 3]

boolList :: [Bool]
boolList = [True, False]

charList :: [Char]
charList = ['H', 'i'] -- type String = [Char] just an alias

string :: String
string = ['H', 'e', 'l', 'l', 'o'] :: [Char]

-- operator for adding an element to the head of a list
str :: String
str = 'H' : "ello" -- "Hello"

intArr :: [Integer]
intArr = 1 : [2, 3, 4] -- [1, 2, 3, 4]

concatListsStr :: [Char]
concatListsStr = str ++ charList ++ string -- HelloHiHello

-- Tuples
-- Tuples can only have fixed type
-- Tuples are heterogenous

pairTuple :: (Integer, Bool)
pairTuple = (2, True)

first :: Integer
first = fst pairTuple

second :: Bool
second = snd pairTuple

noUnaryTuple :: t -> (Integer, t)
noUnaryTuple = (1,) -- it is actually a pair tuple constructor that's partially applied with 1

unitTuple :: ()
unitTuple = () -- unit type

{-
  Types namespace and values namespace never intersect each other
-}

triple :: (Integer, Bool, Char)
triple = (2, True, 'c')

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ sum $ map (\f -> (f p1 - f p2) ^ 2) [fst, snd]

-- Char

char :: Char
char = 'c'

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y
  | all isDigit [x, y] = digitToInt x * 10 + digitToInt y
  | otherwise = 100

test :: Bool
test = isDigit '7' -- True

test2 :: Char
test2 = toLower 'A' -- a

-- Num type class
numInteger :: Integer
numInteger = 5 :: Num a => a

numInt :: Int
numInt = 5 :: Num a => a

integer :: Integer
integer = 5 -- default is Integer

int :: Int
int = 5

-- Fractional type class
fractionalFloat :: Float
-- literal has a polymorph type with `Fractional a` type class constraint
fractionalFloat = 3.0 :: Fractional a => a

fractionalDouble :: Double
-- literal has a polymorph type with `Fractional a` type class constraint
fractionalDouble = 3.0 :: Fractional a => a

double :: Double
double = 5.0 -- default is Double

float :: Float
float = 5.0

bool :: Bool
bool = True
