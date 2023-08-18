import Data.Char (digitToInt)
import Data.List

main :: IO ()
main = do
  print minusInfinity'n'NaN
  print minusInfinity'n'NaN'
  print letInExample0
  print check
  print checkWithAnotherLetSyntax
  print (seqA 301)
  print (rootsWithWhereSyntax 1 2 1)
  print sixteen
  print (sum'n'count (-39))
  print (integration'' sin pi 0)

-- without local binding (duplicate code)
roots :: Double -> Double -> Double
      -> (Double, Double)
roots a b c =
  (
    (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  ,
    (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  )

minusInfinity'n'NaN :: (Double, Double)
minusInfinity'n'NaN = roots 0 1 1

-- with local binding. 'let ... in ...'
roots' :: Double -> Double -> Double
        -> (Double, Double)
roots' a b c =
  let d = sqrt (b ^ 2 - 4 * a * c) in
    (
      (-b - d) / (2 * a)
    ,
      (-b + d) / (2 * a)
    )

minusInfinity'n'NaN' :: (Double, Double)
minusInfinity'n'NaN' = roots 0 1 1

-- let in
letInExample0 :: (Bool, String)
letInExample0 = let x = "Binding" in (True, x) -- (True,"Binding")

roots'' :: Floating b => b -> b -> b -> (b, b)
roots'' a b c =
  let {
    d = sqrt (b ^ 2 - 4 * a * c);
    x1 = (-b - d) / (2 * a);
    x2 = (-b + d) / (2 * a)
    }
  in (x1, x2)

check :: (Integer, Integer, Integer)
check =
  let {
    b = a + 10; -- new definitions in let block can be used inside the same block
    a = 5;
    c = a + b + 10;
  }
  in (a, b, c) -- (5,15,30)

checkWithAnotherLetSyntax :: (Integer, Integer, Integer, String)
checkWithAnotherLetSyntax =
  let
    b = a + 10 -- new definitions in let block can be used inside the same block
    a = 5 -- let definitions should be on the same indentation level
    twoLevelDef = "FirstLevel"
      ++ "secondLevel" -- this is a part of `twoLevelDef` expression
    c = a + b + 10
  in (a, b, c, twoLevelDef) -- (5,15,30,"FirstLevelsecondLevel")

localFunctionBindingsFactorial :: Integer -> Integer
localFunctionBindingsFactorial n
  | n >= 0 = let
      helper acc 0 = acc
      helper acc n = helper (acc * n) (n - 1)
    in
      helper 1 n
  | otherwise = error "arg must be >= 0"

rootsDiff :: Double -> Double -> Double -> Double
rootsDiff a b c = let
  (x1, x2) = roots a b c -- can use pattern matching for local binding
  in x2 - x1

seqA :: Integer -> Integer
seqA n = let
  helper n k0 k1 k2
    | n == 0 = k0
    | otherwise = helper (n - 1) k1 k2 (k2 + k1 - 2 * k0)
  in helper n 1 2 3

rootsWithWhereSyntax :: Double -> Double -> Double -> (Double, Double)
rootsWithWhereSyntax a b c = (x1, x2) where
  x1 = (-b - d) / aTwice
  x2 = (-b + d) / aTwice
  d = sqrt $ b ^ 2 - 4 * a * c
  aTwice = 2 * a

sixteen :: Integer
sixteen = (let x = 2 in x ^ 2) ^ 2

usecaseOfWhereForGuardsFactorial :: Integer -> Integer
usecaseOfWhereForGuardsFactorial n
  | n >= 0 = helper 1 n
  | otherwise = error "arg must be >= 0"
  where
    helper acc 0 = acc -- thanks to 'where' helper function can be used in any guard branch
    helper acc n = helper (acc * n) (n - 1)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n
  | n < 0 = sum'n'count (-n)
  | otherwise = (sumDigits n, len n)
  where
    sumDigits n = toInteger $ sum $ digits n
    len n = genericLength $ digits n
    digits = map digitToInt . show

integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b
  | a <= b = - integration' f b a
  | otherwise = h * sum [ (f x + f (x + h)) / 2
                        | x <- [a, a + h .. b] ]
  where
    h = (b - a) / 1000

integration'' :: (Double -> Double) -> Double -> Double -> Double
integration'' f a b = h * ((f a + f b) / 2 + sum (map f xs))
  where
    n = 1000
    h = (b - a) / n
    xs = [a+h*x | x <- [1..n-1]]
