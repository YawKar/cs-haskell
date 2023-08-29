
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' reduce init [] = init
foldr' reduce init (x : xs) = reduce x $ foldr' reduce init xs

foldr1'' :: (a -> a -> a) -> [a] -> a
foldr1'' reduce [] = error "Main.foldr1'': empty list"
foldr1'' reduce [x] = x
foldr1'' reduce (x : xs) = reduce x (foldr1'' reduce xs)

-- calculate sum of squares of positive elements of the list
sumPositiveSquares :: [Integer] -> Integer
sumPositiveSquares = foldr reduce 0
    where
        -- reduce el = (+) (if el > 0 then el ^ 2 else 0) -- another working implementation
        reduce el
            | el > 0 = (+) (el ^ 2)
            | otherwise = id

lengthList :: [a] -> Int
lengthList = foldr reduce 0
    where
        reduce _ = (+) 1

sumOdd :: [Integer] -> Integer
sumOdd = foldr reduce 0
    where
        reduce el
            | odd el = (+) el
            | otherwise = id

revList :: [a] -> [a]
revList = foldl (flip (:)) []

idForLists :: [a] -> [a]
idForLists = foldr (:) []

head' :: [a] -> a
head' = foldr const undefined
