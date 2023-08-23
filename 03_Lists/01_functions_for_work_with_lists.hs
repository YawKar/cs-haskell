module Demo where

import Data.Function (on)

main :: IO ()
main = do
  print $ "Empty list: " ++ show (emptyList :: [Bool]) -- need to monomorphize
  print $ "Add 3 as a head: " ++ show addNewHead
  print $ "Add 4 as next head: " ++ show next
  print consOperatorIsRightAssociative
  print $ cons42 [43, 44]
  print $ addTwoElements 2 12 [85, 0, 6]

{- There are 2 main ways to construct lists -}
-- 1. Using empty list constructor
emptyList :: [a]
emptyList = []
-- 2. Using cons operator
addNewHead :: [Integer]
addNewHead = 3 : emptyList -- [3]
next :: [Integer]
next = 4 : addNewHead -- [4, 3]
consOperatorIsRightAssociative :: [String]
consOperatorIsRightAssociative = "Cons" : "Operator" : "Is" : "Right" : "Associative" : [];

cons42 :: Num a1 => [a1] -> [a1]
cons42 = (42:) -- function that adds 42 in the head of any given list

addTwoElements :: Num a => a -> a -> [a] -> [a]
addTwoElements = (.) `on` (:) -- addTwoElements first second = (:) first . (:) second

nTimes :: a -> Int -> [a]
nTimes value n
  | n == 0 = []
  | otherwise = value : nTimes value (n - 1)
{-
Other implementations:
nTimes = flit replicate
nTimes = take n $ repeat x
-}
myRepeat :: a -> [a]
myRepeat val = val : myRepeat val

-- We have 4 main functions for list destructioning: head & tail, init & last
firstElem :: Integer
firstElem = head [1, 2, 3] -- 1 (panics)
withoutFirstElem :: [Integer]
withoutFirstElem = tail [1, 2, 3] -- [2, 3] (panics)
lastElem :: Integer
lastElem = last [1, 2, 3] -- 3 (panics)
withoutLastElem :: [Integer]
withoutLastElem = init [1, 2, 3] -- [1, 2] (panics)

second :: [a] -> a
second = head . tail -- returns second elem of the given list

-- We also have pattern matching for list destructioning
head' :: [a] -> a
head' (a : _) = a -- head' ((:) a _)
head' _ = error "List should not be empty"

tail' :: [a] -> [a]
tail' (_ : xs) = xs -- tail' ((:) _ xs)
tail' _ = error "List should not be empty"

second' :: [a] -> a
second' (_ : a : _) = a
second' _ = error "List should be at least 2 length size"

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

infixr 5 #++#
(#++#) :: [a] -> [a] -> [a]
(#++#) [] b = b
(#++#) (x : xs) b = x : (xs #++# b)

null' :: [a] -> Bool
null' [] = True
null' _ = False

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

last' :: [a] -> a
last' [] = error "List should not be empty"
last' [x] = x
last' (_ : xs) = last' xs

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x : xs) = x : init' xs

sum' :: Num a => [a] -> a
sum' [x] = x
sum' (x : xs) = x + sum' xs
sum' [] = error "List should not be empty!"
-- sum' = foldr1 (+) -- another possible implementation

product' :: Num a => [a] -> a
product' [x] = x
product' (x : xs) = x * product' xs
product' [] = error "List should not be empty!"
-- product' = foldr1 (*) -- another possible implementation

maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)
maximum' [] = error "List should not be empty!"

minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x : xs) = min x (minimum' xs)
minimum' [] = error "List should not be empty!"

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x : xs) = helper [x] xs
  where
    helper cur (x : xs) = helper (x : cur) xs
    helper cur _ = cur

reverse'' :: [a] -> [a]
reverse'' l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

zippedHello :: [(Integer, Char)]
zippedHello = zip [1, 2, 3] "Hello" -- [(1,'H'),(2,'e'),(3,'l')]

zip' :: [a] -> [b] -> [(a, b)]
zip' (a:as) (b:bs) = (a, b) : zip' as bs
zip' _ _ = []

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' (a:as) (b:bs) (c:cs) = (a, b, c) : zip3' as bs cs
zip3' _ _ _ = []

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a, b) : abs) = (a : as, b : bs)
  where
    as = fst unzippedTail
    bs = snd unzippedTail
    unzippedTail = unzip' abs

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' [] = ([], [])
unzip'' ((a, b) : abs) =
  let
    (as, bs) = unzip'' abs
  in
    (a : as, b : bs)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 a b c = (asum + bsum + csum) : sum3 as bs cs
  where
    safeHead xs =
      case xs of
        [] -> Nothing
        (x : xs) -> Just x
    fromMaybe defVal maybe =
      case maybe of
        Just x -> x
        Nothing -> defVal
    as =
      case a of
        [] -> []
        (_:xs) -> xs
    bs =
      case b of
        [] -> []
        (_:xs) -> xs
    cs =
      case c of
        [] -> []
        (_:xs) -> xs
    asum = fromMaybe 0 (safeHead a)
    bsum = fromMaybe 0 (safeHead b)
    csum = fromMaybe 0 (safeHead c)

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = collectGroups x [x] xs
  where
    collectGroups curTarget curCollected (x : xs)
      | curTarget == x = collectGroups curTarget (x : curCollected) xs
      | otherwise = curCollected : collectGroups x [x] xs
    collectGroups curTarget curCollected [] = [curCollected]

groupElems' :: Eq a => [a] -> [[a]]
groupElems' [] = []
groupElems' (x : xs) =
  case groupElems' xs of
    [] -> [[x]]
    (g : gs)
      | x == head g -> (x : g) : gs
      | otherwise -> [x] : g : gs
