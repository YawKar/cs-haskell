
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' reduce init [] = init
foldl'' reduce init (x : xs) = foldl'' reduce (reduce init x) xs

foldlStrict :: (b -> a -> b) -> b -> [a] -> b
foldlStrict reduce init [] = init
foldlStrict reduce init (x : xs) = nextInit `seq` foldlStrict reduce nextInit xs
    where
        nextInit = reduce init x

anyUsingFoldr :: (a -> Bool) -> [a] -> Bool
anyUsingFoldr p = foldr (\x acc -> p x || acc) False

sumAndProduct :: Num a => [a] -> (a, a)
sumAndProduct = foldr reduce (0, 1)
    where
        reduce el (sum, prod) = (sum + el, prod * el)

meanList :: [Double] -> Double
meanList = calcMean . foldr reduce (0, 0)
    where
        calcMean (sum, n) = sum / n
        reduce el (sum, n) = (sum + el, succ n)

evenOnly :: [a] -> [a]
evenOnly = reverse . snd . foldl reduce (False, [])
    where
        reduce (True, acc) el = (False, el : acc)
        reduce (False, acc) el = (True, acc)

evenOnlyForInfinite :: [a] -> [a]
evenOnlyForInfinite = helper False
    where
        helper _ [] = []
        helper False (x : xs) = helper True xs
        helper True (x : xs) = x : helper False xs
