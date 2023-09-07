module Demo () where
import Data.Char (isDigit)

-- Maybe monad
data Maybe' a
    = Nothing'
    | Just' a
    deriving (Eq, Ord, Show)

instance Functor Maybe' where
    fmap f Nothing' = Nothing'
    fmap f (Just' x) = Just' $ f x

instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    _ <*> Nothing' = Nothing'
    Just' transform <*> Just' x = Just' $ transform x

instance Monad Maybe' where
    Nothing' >>= _ = Nothing'
    Just' x >>= kleisli = kleisli x

    Nothing' >> _ = Nothing'
    Just' _ >> m2 = m2

instance MonadFail Maybe' where
    fail _ = Nothing'

type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers = [("Bill", "John"),
           ("Ann", "John"),
           ("John", "Piter")]
mothers = [("Bill", "Jane"),
           ("Ann", "Jane"),
           ("John", "Alice"),
           ("Jane", "Dorothy"),
           ("Alice", "Mary")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

getGMByMother :: Name -> Maybe Name
getGMByMother person = getM person >>= getM
getGMByFather :: Name -> Maybe Name
getGMByFather person = do
    father <- getF person
    getM father

getGFByMother :: Name -> Maybe Name
getGFByMother person = do
    mother <- getM person
    getF mother
getGFByFather :: Name -> Maybe Name
getGFByFather person = getF person >>= getF

grandmas :: Name -> Maybe (Name, Name)
grandmas person = do
    father <- getF person
    grandMotherByF <- getM father

    mother <- getM person
    grandMotherByM <- getM mother

    return (grandMotherByM, grandMotherByF)

-- task
data Token
    = Number Int
    | Plus
    | Minus
    | LeftBrace
    | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str = case str of
    "+" -> Just Plus
    "-" -> Just Minus
    "(" -> Just LeftBrace
    ")" -> Just RightBrace
    _ ->
        if all isDigit str
            then Just $ Number $ read str
            else Nothing

tokenize :: String -> Maybe [Token]
tokenize str = helper $ words str
    where
        helper [] = do
            return []
        helper (x : xs) = do
            token <- asToken x
            restTokens <- helper xs
            return $ token : restTokens

-- List monad

packed4 :: [Integer]
packed4 = return 4 :: [Integer] -- [4] , bc return x =  [x]

tripleEachElement :: [Integer]
tripleEachElement = [1, 2] >>= (\x -> [x, x, x]) -- [1, 1, 1, 2, 2, 2], so basically it's a concatenation
-- (\x -> [x, x, x]) 1 <> (\x -> [x, x, x]) 2 ~> [1, 1, 1] <> [2, 2, 2] = [1, 1, 1, 2, 2, 2]
-- ~= concatMap
-- bc xs >>= k = concat (map k xs)
-- fail _ = []
tripleEachElement' = concatMap (\x -> [x, x, x]) [1, 2] -- [1, 1, 1, 2, 2, 2]

complexLogic0 :: [Integer]
complexLogic0 = [1, 2, 3] >>= (\x -> [x | odd x]) -- [1, 3]
complexLogic0' = concatMap (\x -> [x | odd x]) [1, 2, 3] -- [1, 3]
complexLogic1 :: [Integer]
complexLogic1 = [1, 2, 3] >>= (\x -> if even x then [x] else []) -- [2]
complexLogic1' = concatMap (\x -> if even x then [x] else []) [1, 2, 3] -- [2]

list :: [(Integer, Integer)]
list = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
list' :: [(Integer, Integer)]
list' = do
    x <- [1, 2, 3]
    y <- [4, 5, 6]
    return (x, y)
list'' :: [(Integer, Integer)]
list'' = do
    [1, 2, 3] >>= \x ->
        [4, 5, 6] >>= \y ->
        return (x, y)

data Board = Board

nextPositions :: Board -> [Board]
nextPositions = undefined

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
    | n < 0 = []
    | n == 0 = [b | pred b]
    | otherwise = do
        nxBoard <- nextPositions b
        nextPositionsN nxBoard (n - 1) pred

{-
How can we perform conditional (branching) logic inside monadic calculations?
-}
lst = [(x, y) | x <- [1, 2, 3], y <- [1, 2], x /= y]

lst' = do
    x <- [1, 2, 3]
    y <- [1, 2]
    True <- return $ x /= y -- will fail the monadic calculation if x == y and eventually become []
    return (x, y)

lst'' = -- desugared
    [1, 2, 3]       >>= \x ->
    [1, 2]          >>= \y ->
    return (x /= y) >>= \result ->
        case result of
            True -> return (x, y)
            False -> fail ""

lstAlternative = do
    x <- [1, 2, 3]
    y <- [1, 2]
    k <- if x /= y then "Z" else []
    return (x, y, k)

lstAlternative' :: [(Integer, Integer, Char)]
lstAlternative' = -- desugared
    [1, 2, 3]                     >>= (\x ->
    [1, 2]                        >>= (\y ->
    (if x /= y then "Z" else [])  >>= (\k ->
    return (x, y, k))))

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    c <- [1..x]
    b <- [1..c]
    a <- [1..b]
    -- if a ^ 2 + b ^ 2 == c ^ 2 then "A" else [] -- first variant
    True <- return $ a ^ 2 + b ^ 2 == c ^ 2 -- alternate variant
    return (a, b, c)
