
data Bool' = True' | False' deriving (Show, Eq, Read, Enum)

alwaysTrue :: Num a => a -> Bool'
alwaysTrue n = True'

not' :: Bool' -> Bool'
not' True' = False'
not' False' = True'

-- :set -fwarn-incomplete-patterns

if' :: Bool' -> a -> a -> a
if' True' x _ = x
if' False' _ y = y

five :: IO ()
five = print (if' (not' False') 5 13)

data Color = Red | Green | Blue

instance Show Color where
    -- show Red = "Red"
    -- show Green = "Green"
    -- show Blue = "Blue"
    show c = case c of
        Red -> "Red"
        Green -> "Green"
        Blue -> "Blue"

intToChar :: Int -> Char
intToChar int = case int of
    0 -> '0'
    1 -> '1'
    2 -> '2'
    3 -> '3'
    4 -> '4'
    5 -> '5'
    6 -> '6'
    7 -> '7'
    8 -> '8'
    9 -> '9'

isZ :: Char -> Bool
isZ 'Z' = True
isZ _ = False

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False

charToInt :: Char -> Int
charToInt c = case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9

stringToColor :: String -> Color
stringToColor str = case str of
    "Red" -> Red
    "Green" -> Green
    "Blue" -> Blue

foo 1 2 = 3
foo 0 _ = 5

bar (1, 2) = 3
bar (0, _) = 5

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp = undefined

lessThanError :: LogLevel -> Bool
lessThanError c = case cmp c Error of
    LT -> True
    _ -> False

data Result = Fail | Success

data SomeData = SomeData

doSomeWork :: SomeData -> (Result, Int)
doSomeWork = undefined

processData :: SomeData -> String
processData datum =
    case doSomeWork datum of
        (Success, _) -> "Success"
        (Fail, errorCode) -> "Fail: " ++ show errorCode
