main :: IO ()
main = do
  print checkItOut
  print (swapPairElements (1, 2))

infixr 9 ^.^ -- the same as '.'
(^.^) :: (a -> b) -> (c -> a) -> c -> b
(^.^) f2 f1 x = f2 (f1 x)

checkItOut :: Integer
checkItOut = (^2) ^.^ (+5) $ 10 -- 225 <~ ((^2) ((+5) 10))

doItYourself :: Double -> Double
doItYourself = f . g . h
  where
    f = logBase 2
    g = (^3)
    h = max 42

emptyPolymorphicList :: [a] -- lists are polymorphic by the type of their elements
emptyPolymorphicList = []

anotherSyntaxForPairConstructing :: (Bool, Integer)
anotherSyntaxForPairConstructing = (,) True 3

anotherSyntaxForTripletConstructing :: (Bool, Integer, [Char])
anotherSyntaxForTripletConstructing = (,,) True 3 "Hello"

anotherSyntaxForQuadrupleConstructing :: (Bool, Integer, [Char])
anotherSyntaxForQuadrupleConstructing = (,,) True 3 "Hello"

mixfixStyleConstruction :: (Bool, Integer)
mixfixStyleConstruction = (True, 1)
-- And any sized tuple can be constructed using standard function call style (prefix)

tuplesArePolymorphicByElements'Types :: a -> b -> (a, b)
tuplesArePolymorphicByElements'Types = (,)

avg :: (Double, Double) -> Double
avg p = uncurry' (+) p / 2

curriedAvg :: Double -> Double -> Double
curriedAvg = curry' avg

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x1, x2) = f x1 x2

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

curriedIdIsPairConstructor :: a -> b -> (a, b)
curriedIdIsPairConstructor = curry' id

uncurriedFlippedConstIsSnd :: (a, b) -> b
uncurriedFlippedConstIsSnd = uncurry' (flip const)

swapPairElements :: (a, b) -> (b, a)
swapPairElements = uncurry' (flip (,))
