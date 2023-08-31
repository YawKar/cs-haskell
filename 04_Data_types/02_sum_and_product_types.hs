import Data.Ratio qualified

data Point = Point Double Double deriving (Eq, Show)

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
-- distance (Point ax ay) (Point bx by) = sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2)
distance (Point ax ay) (Point bx by) = distanceToOrigin (Point (ax - bx) (ay - by))

data Roots = Roots Double Double | None
    deriving (Eq, Show)

roots :: Double -> Double -> Double -> Roots
roots a b c
    | discr >= 0 = Roots x1 x2
    | otherwise = None
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c

data Shape
    = Circle Double
    | Rectangle Double Double

square :: Double -> Shape
square side = Rectangle side side

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False

area :: Shape -> Double
area shape = case shape of
    Circle r -> pi * r ^ 2
    Rectangle a b -> a * b

data Result'
    = Success
    | Fail Int

instance Show Result' where
    show :: Result' -> String
    show res = case res of
        Success -> "Success"
        Fail errCode -> "Fail: " ++ show errCode

fiveSixths :: Data.Ratio.Ratio Integer
fiveSixths = 2 Data.Ratio.% 3 + 1 Data.Ratio.% 6;

-- Whole numbers can be represented as a list of bits each of which is either 0 or 1 and a sign
data Bit = Zero | One deriving (Eq)
data Sign = Minus | Plus deriving (Eq)
data Z = Z Sign [Bit] deriving (Eq)
emptyZ = Z Plus []

toInteger' :: Z -> Integer
toInteger' (Z sign bits) =
    if sign == Minus
        then -value
        else value
    where
        value = sum (zipWith toPower bits [0..])
        toPower Zero _ = 0
        toPower One pos = 2 ^ pos

fromInteger' :: Integer -> Z
fromInteger' value =
    if value < 0
        then Z Minus (toBits (-value))
        else Z Plus (toBits value)
    where
        toBits 0 = []
        toBits x =
            if even x
                then Zero : toBits (x `div` 2)
                else One : toBits (x `div` 2)

add :: Z -> Z -> Z
add z1 z2 = fromInteger' (toInteger' z1 + toInteger' z2)

mul :: Z -> Z -> Z
mul z1 z2 = fromInteger' (toInteger' z1 * toInteger' z2)

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- f1 *** f2 = \(x1, x2) -> (f1 x1, f2 x2)
-- (***) f g (x, y) = (f x, g y) -- won't work if (x, y) was passed as undefined
(***) f g ~(x, y) = (f x, g y) -- but this one will work even with (x, y) passed as undefined

foo ~False = 0 -- all calls will fall-in there because ~False pattern matching is lazy
foo True = 1 -- redundant
zero = foo True
