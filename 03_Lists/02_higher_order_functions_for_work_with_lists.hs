import Data.Char (isDigit, isUpper)
import Data.List (partition)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (x : xs)
  | predicate x = x : filter' predicate xs
  | otherwise = filter' predicate xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' predicate (x : xs)
  | predicate x = x : takeWhile' predicate xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' predicate xs@(x : xs') -- local alias for complex pattern
  | predicate x = dropWhile' predicate xs' -- xs' is a destructured tail
  | otherwise = xs -- xs is an alias for the list destructured through a complex pattern

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' predicate xs = (takeWhile' predicate xs, dropWhile' predicate xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' predicate = span' (not . predicate)

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    (lhs, rhs) = partition (<x) xs

qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x : xs) = qsort' lhs ++ [x] ++ qsort' rhs
  where
    lhs = filter (<x) xs
    rhs = filter (>=x) xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss
-- concat' = foldr (++) [] -- another implementation

concatMap' :: (a -> [b]) -> [a] -> [b] -- may be Foldable t => t a
concatMap' f = concat' . map' f

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = helperIterate [] xs
  where
    helperIterate lhs [] = []
    helperIterate lhs (h : rhs) = map (h:) (perms $ lhs ++ rhs) ++ helperIterate (h : lhs) rhs -- very slow

perms' :: [a] -> [[a]]
perms' [] = [[]]
perms' [x] = [[x]]
perms' (x : xs) = concatMap (insertElem x) (perms' xs)
  where
    insertElem x [] = [[x]]
    insertElem x ys@(y : yss) = (x : ys) : map (y:) (insertElem x yss) -- pretty fast

and', or' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs
-- and' = foldr (&&) True -- another implementation
or' [] = False
or' (x : xs) = x || or' xs
-- or' = foldr (||) False -- another implementation

all' :: (a -> Bool) -> [a] -> Bool
all' p = and' . map' p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or' . map' p

reverseWordsInSentence :: String -> String
reverseWordsInSentence = unwords . map reverse . words

delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

zipThroughZipWith' :: [a] -> [b] -> [(a, b)]
zipThroughZipWith' = zipWith' (,)

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 ((max .) . max)

max3' :: Ord a => a -> a -> a -> a
max3' = (.) (max .) max
