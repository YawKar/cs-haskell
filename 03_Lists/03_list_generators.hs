import GHC.Natural (Natural)

-- unproductive divergent program
bot :: Bool
bot = not bot -- infinite Bool recursion (Bool because not :: Bool -> Bool)

-- productive divergent program
ones :: [Integer]
ones = 1 : ones -- infinite list of ones

nats :: Natural -> [Natural]
nats n = n : nats (n + 1)

first10Elems :: [Natural]
first10Elems = take 10 (nats 1)

fortyTwo :: Natural
fortyTwo = head $ nats 42

fibStream :: [Integer]
fibStream = 0 : zipWith (+) fibStream (1 : fibStream)

fibStream' :: [Integer]
fibStream' = 0 : 1 : helper 0 1
  where
    helper x0 x1 = x2 : helper x1 x2
      where
        x2 = x0 + x1

repeat' :: a -> [a]
repeat' x = x : repeat' x
-- repeat' x = xs
--   where
--     xs = x : xs

replicate' :: Int -> a -> [a]
replicate' n = take n . repeat'

cycle' :: [a] -> [a]
cycle' [] = error "cycle': empty list"
cycle' xs = xs ++ cycle' xs
-- cycle' xs = ys where ys = xs ++ ys

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

from1to10 :: [Integer]
from1to10 = [1..10] -- just a sugar for `enumFromTo :: Enum a => a -> a -> [a]`
from1to10' :: [Integer]
from1to10' = enumFromTo 1 10

from1to10Step2 :: [Integer]
from1to10Step2 = [1,3..10] -- [1, 3, 5, 7, 9]; just a sugar for `enumFromThenTo :: Enum a => a -> a -> a -> [a]`
from1to10Step2' :: [Integer]
from1to10Step2' = enumFromThenTo 1 3 10

infiniteFrom1 :: [Integer]
infiniteFrom1 = [1..] -- just a sugar for `enumFrom :: Enum a => a -> [a]`
infiniteFrom1' :: [Integer]
infiniteFrom1' = enumFrom 1

infiniteFrom1Step2 :: [Integer]
infiniteFrom1Step2 = [1,3..] -- just a sugar for `enumFromThen :: Enum a => a -> a -> [a]`
infiniteFrom1Step2' :: [Integer]
infiniteFrom1Step2' = enumFromThen 1 2

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  toEnum x = Odd (fromIntegral x)
  fromEnum (Odd x) = fromIntegral x
  enumFrom x = x : enumFrom (succ x)
  enumFromThen x0@(Odd x0val) x1@(Odd x1val) = x0 : enumFromThen x1 (Odd (x1val + diff))
    where
      diff = x1val - x0val
  enumFromTo x@(Odd xval) y@(Odd yval)
    | xval > yval = []
    | otherwise = x : enumFromTo (succ x) y
  enumFromThenTo x0@(Odd x0val) x1@(Odd x1val) x2@(Odd x2val)
    | if diff < 0 then x0val < x2val else x0val > x2val = []
    | otherwise = x0 : enumFromThenTo x1 (Odd (x1val + diff)) x2
    where
      diff = x1val - x0val
