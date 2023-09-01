import Data.List (elemIndex)
import Data.Char (isDigit, isSpace, isNumber)
import Data.Maybe (isJust, fromMaybe)

data CoordD = CoordD Double Double deriving (Eq, Show)
data CoordI = CoordI Int Int deriving (Eq, Show)

data Coord a -- type constructor (kind: * -> *)
    = Coord a a -- data constructor (type: a -> a -> Coord a)
    deriving (Show)

-- task 2
distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ( (x1 - x2) ^ 2 + (y1 - y2) ^ 2 )

manhattanDistance :: Coord Int -> Coord Int -> Int
manhattanDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- task 3
getCenter :: Double -> Coord Int -> Coord Double
getCenter cellSide (Coord cellX cellY) = Coord (cellSide * fromIntegral cellX + cellSide / 2) (cellSide * fromIntegral cellY + cellSide / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell cellSide (Coord x y) = Coord (floor (x / cellSide)) (floor (y / cellSide))

-- Standard parametric types
twice :: a -> [] a -- the same as: a -> [a]
twice x = [x, x]

thrice :: a -> (,,) a a a -- the same as: a -> (a, a, a) -- so-called mixfix style
thrice x = (x, x, x) -- the same as: (,,) x x x

id' :: (->) a a -- the same as: a -> a
id' x = x

konst :: (->) a ((->) b a)
konst x y = x

{-
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
-}

roots :: Double -> Double -> Double -> Either [Char] (Double, Double)
roots a b c
    | discr >= 0 = Right (x1, x2)
    | otherwise = Left "Negative discriminant"
    where
        discr = b ^ 2 - 4 * a * c
        d = sqrt discr
        x1 = root (-d)
        x2 = root d
        root d = (-b + d) / (2 * a)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs)
    | isDigit x = Just x
    | otherwise = findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
    Just found -> found
    _ -> 'X'

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just x) = [x]

listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x : xs) = Just x

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Eq, Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Eq, Show)

parsePerson :: String -> Either Error Person
parsePerson = setFields . lines
    where
        setFields fields
            | anyParseErrors fields' = Left ParsingError
            | anyFieldIsMissing fields' = Left IncompleteDataError
            | otherwise = assignPerson Person{} fields'
            where
                fields' = map (fromMaybe undefined) . filter isJust . map splitFieldToKV $ fields

        assignPerson :: Person -> [(String, String)] -> Either Error Person
        assignPerson person [] = Right person
        assignPerson person ((fieldName, fieldValue) : restFields) =
            case newPerson of
                Right p -> assignPerson p restFields
                err -> err
            where
                newPerson = case fieldName of
                    "firstName" -> Right person { firstName = fieldValue }
                    "lastName" -> Right person { lastName = fieldValue }
                    "age" ->
                        if all isNumber fieldValue
                            then Right person { age = read fieldValue }
                            else Left (IncorrectDataError fieldValue)
                    _ -> Right person

        anyFieldIsMissing :: [(String, String)] -> Bool
        anyFieldIsMissing = not . and . detuplify3 . foldr reduce (thrice False)
            where
                detuplify3 (a, b, c) = [a, b, c]
                reduce (fieldName, fieldValue) state@(isFn, isLn, isAge) =
                    case fieldName of
                        "firstName" -> (True, isLn, isAge)
                        "lastName" -> (isFn, True, isAge)
                        "age" -> (isFn, isLn, True)
                        _ -> state

        anyParseErrors :: [(String, String)] -> Bool
        anyParseErrors fields
            | not (all isFieldSyntacticallyCorrect fields) = True
            | otherwise = False

        isFieldSyntacticallyCorrect :: (String, String) -> Bool
        isFieldSyntacticallyCorrect (fieldName, fieldValue) = not (any null [fieldName, fieldValue])

        splitFieldToKV :: String -> Maybe (String, String)
        splitFieldToKV field =
            case elemIndex '=' field of
                Nothing -> Nothing
                Just eqPosition ->
                    Just (trim $ take eqPosition field, trim $ drop (eqPosition + 1) field)

        trim :: String -> String
        trim = f . f
            where f = reverse . dropWhile isSpace

{-
ghci> :k Int
Int :: *
ghci> :k Maybe
Maybe :: * -> *
ghci> :k Maybe Int
Maybe Int :: *
ghci> :k Either
Either :: * -> * -> *
ghci> :k Either Int String
Either Int String :: *
ghci> :k []
[] :: * -> *
ghci> :k [Int]
[Int] :: *
ghci> :k (,)
(,) :: * -> * -> *
ghci> :k (,,)
(,,) :: * -> * -> * -> *

only * kinds have values that inhabit them
-}

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

{-
We can force strict evaluation of values under parametric types on the data constructor level
-}

data CoordLazy a = CoordLazy a a
    deriving Show

data CoordStrict a = CoordStrict !a !a
    deriving Show

getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x
is3 = getXLazy (CoordLazy 3 undefined)

getXStrict :: CoordStrict a -> a
getXStrict (CoordStrict x _) = x
isDivergentCalculation = getXStrict (CoordStrict 3 undefined)

{-
Convention: any infix data constructor should begin with ':' (an equvivalent of a Big Letter)
data Complex a = !a :+ !a
data Ratio a = !a :% !a
-}
