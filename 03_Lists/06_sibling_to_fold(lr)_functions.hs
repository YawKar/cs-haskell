import Data.List (foldl1', unfoldr)

foldr1My :: (a -> a -> a) -> [a] -> a
foldr1My _ [] = error "Main.foldr1My: empty list"
foldr1My _ [x] = x
foldr1My op (x : xs) = x `op` foldr1My op xs
-- foldr1My op (x : xs) = foldr op x xs -- implementation using foldr

foldl1My :: (a -> a -> a) -> [a] -> a
foldl1My _ [] = error "Main.foldl1My: empty list"
foldl1My op (x : xs) = foldl op x xs

min' :: Ord a => [a] -> a
min' = foldl1' min

max' :: Ord a => [a] -> a
max' = foldl1' max

lastElem :: [a] -> a
lastElem = foldl1 (\_ x -> x)
-- lastElem = foldl1 seq -- there's a huge flaw because it will evaluate to WHNF unimportant elements on its way to the last element
-- lastElem = foldl1 (flip const)

scanlMy :: (b -> a -> b) -> b -> [a] -> [b]
scanlMy f ini [] = [ini]
scanlMy f ini (x : xs) = ini : scanlMy f (ini `f` x) xs

first10Factorials :: [Integer]
first10Factorials = scanl (*) 1 [1..10]

facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1..]

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

first10PartialSums :: [Integer]
first10PartialSums = partialSums [1..10]

exponent' :: Int -> Double
exponent' precision = (!! precision) . partialSums . map (**(-1)) $ facs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' op ini [] = [ini]
scanr' op ini (x : xs) = x `op` rx : r
    where
        r@(rx : _) = scanr' op ini xs

unfold' :: (b -> (a, b)) -> b -> [a]
unfold' f ini = applied : unfold' f nextIni
    where
        (applied, nextIni) = f ini
-- unfold' f ini =
--     let
--         (applied, nextIni) = f ini
--     in
--         applied : unfold' f nextIni

areEqual :: Bool
areEqual = take 10 (unfold' (\x -> (sin x, sin x)) 1) == (tail . take 11 . iterate sin) 1 -- True

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold' (\x -> (x, f x))

data Maybe' a
    = Nothing'
    | Just' a
    deriving (Show, Eq)

find' :: (a -> Bool) -> [a] -> Maybe' a
find' _ [] = Nothing'
find' p (x : xs)
    | p x = Just' x
    | otherwise = find' p xs

lookup' :: Eq a => a -> [(a, b)] -> Maybe' b
lookup' key [] = Nothing'
lookup' key ((key', value') : xs)
    | key == key' = Just' value'
    | otherwise = lookup' key xs

bLetter :: Maybe' Char
bLetter = lookup' 2 $ zip [1..5] ['a'..'z'] -- Just 'b'

unfoldr' :: (b -> Maybe' (a, b)) -> b -> [a] -- can be stopped using condition (return Nothing)
unfoldr' f ini = helper (f ini)
    where
        helper (Just' (el, nIni)) = el : unfoldr' f nIni
        helper Nothing' = []

revRange :: (Char, Char) -> [Char]
revRange (left, right) = unfoldr unfolder right
    where
        unfolder curC =
            if curC >= left
                then Just (curC, pred curC)
                else Nothing
