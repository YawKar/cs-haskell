
{-
There are two types of polymorphism in Haskell:
1. Parametric polymorphism
    # refers to when the type of a value contains one or more [unconstrained] type variables
    # so that the value may adopt any type that results from substituting those variables
    # with concrete types
2. Ad-hoc polymorphism
    # refers to when a value is able to adopt any one of several types because it, or a value it uses,
    # has been given a separate definition for each of those types;
    # they have more in common with interfaces, in that they specify a series of methods or values by their
    # type signature, to be implemented by an instance declaration
    # (Rust's traits are very similar to type classes in Haskell)
-}

id' :: p -> p -- one of the simplest polymorphic function example
id' x = x

const' :: a -> b -> a
const' val x = val -- evaluates into a function that returns the same `val` value for any provided `x`

alwaysReturn2 :: t -> Integer
alwaysReturn2 = const' 2 -- type `t` is arbitrary and doesn't matter

undefinedInteger :: Integer
undefinedInteger = undefined -- `undefined` inhabits any type and is a highest polymorhpic value (a bottom type)
undefinedString :: String
undefinedString = undefined -- `undefined` has a type of `GHC.Stack.Types.HasCallStack => a`

{-
`undefined` and `error` are used to panic the whole program
and must have the bottom type after reduction to be usable anywhere
-}

-- `error` has a type of `GHC.Stack.Types.HasCallStack => [Char] -> a`
-- where [Char] is an error message
errorInteger :: Integer
errorInteger = error "error is another `bottom` type that inhabits any type"
errorString :: String
errorString = error "error is another `bottom` type that inhabits any type"

getSecondFrom :: a -> b -> c -> b
getSecondFrom a b c = b

mono :: Char -> Char -- monomorphic function
mono c = c

-- partially constrained (by the first argument) polymorphic (by the second argument) function
semiMono :: Char -> a -> Char
semiMono c x = c

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' op f x y = f x `op` f y

sumSquares :: Num a => a -> a -> a
sumSquares = (+) `on'` (^2)

multSecond :: (a, Integer) -> (a, Integer) -> Integer
multSecond = g `on'` h
  where
    g = (*)
    h = snd

f :: Integer
f = (\x -> 2 * x + 7) 10

lenVec :: Floating a => a -> a -> a
lenVec x y = sqrt (sumSquares x y)

lenVec' :: Floating a => a -> a -> a
lenVec' = \x y -> sqrt (sumSquares x y)

sumFstFst :: ((Integer, b1), b2) -> ((Integer, b1), b2) -> Integer
sumFstFst = (+) `on'` (fst . fst)

trySumFstFst :: Integer
trySumFstFst = sumFstFst ((1, 2), (3, 4)) ((3, 4), (5, 6)) -- 4

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 ternaryOp mapper a b c = mapper a `ternaryOp` mapper b $ mapper c

-- Exercise from comments
flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

{-
Why type of flippedId is: b -> (b -> c) -> c ?
Perhaps, let's examine the type of `id`: d -> d

1. Because `id` is the first argument to `flip`
we have this relationship: d -> d = a -> b -> c

2. Let's rewrite it like that: d -> d = a -> (b -> c)

3. We see that on the both sides of the equation
there is a `* -> *` kind of type

4. Using the knowledge that `d -> d` has the kind `* -> *`,
we can conclude that `*` must be the same

5. Therefore: a = b -> c (* -> * and * is the same)

6. Substituting: ((b -> c) -> b -> c) -> b -> (b -> c) -> c

7. Apply `id`: ((b -> c) -> b -> c) -> b -> (b -> c) -> c $ ((b -> c) -> (b -> c))
~> b -> (b -> c) -> c
-}
flippedId :: b -> (b -> c) -> c
flippedId = Main.flip id
