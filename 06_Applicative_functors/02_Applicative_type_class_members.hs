module Demo where

import Control.Monad.Identity (Identity (Identity))
import Data.List (singleton)
import Data.Monoid (Sum (Sum))
import GHC.Base (liftA2)

fs :: [Integer -> Integer]
fs = [(* 2), (+ 3), (4 -)]

as :: [Integer]
as = [1, 2]

standardBehaviour :: [Integer]
standardBehaviour = fs <*> as -- [2, 4, 4, 5, 3, 2]

{-
-- Standard implementation for [] from GHC.Base (4.17.2.0)
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

desugaring:
  fs >>= (\f ->
    xs >>= (\x ->
      f x))
-}

{-
offtopic:
I just found out very funny behvaiour of `id` functions
when applied as binary operators.

So, basically, if we section `id` operator by providing its first argument:
  : (10 `id`)
it will indeed work as expected, to the type of (10 `id`) will be Integer.

But if we try to provide only its second argument:
  : (`id` 10)
things become funny as `id` sees that it has some 'second' argument and now
it needs something in the first argument to perform a 'binary action`.

The type of (`id` 10) is `(Integer -> c) -> c`
  : and the type of `id` here is
    : `id` :: (Integer -> c) -> Integer -> c

In fact, it is exactly how `$` operator works:
  : `$` :: (a -> b) -> a -> b

And we even can implement it using `id`:
  : ($) f x = f `id` x
-}
infixr 9 <!*>

(<!*>) :: p -> p
(<!*>) d = d

id' :: a -> a
id' x = x

k0 :: Integer
k0 = (10 `id`)

k = (10 <!*>)

k' = (\x -> show x) `id` ((* 2) `id` 10)

k'' = (\x -> show x) $ 10

implementDollarThroughId :: (a -> b) -> a -> b
implementDollarThroughId f x = f `id` x

-- OFFTOPIC ENDS

newtype ZipList a = ZipList {getZipList :: [a]}
  deriving (Eq, Show)

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f (ZipList content) = ZipList $ fmap f content

instance Applicative ZipList where
  -- This implementation is bad.
  -- Because it breaks the first law of Applicative (Identity law):
  -- `pure id <*> ZipList [1, 2]` will be `ZipList [1]`, not `ZipList [1, 2]`
  -- pure = ZipList . repeat -- this is a good implementation, because it respects the first (Identity) law (and all other)
  pure :: a -> ZipList a
  pure = ZipList . singleton

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  ZipList xs <*> ZipList ys = ZipList $ zipWith id xs ys

{-
id :: a -> a
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
a -> a === a -> (b -> c)
a === b -> c
zipWith*2 :: ((b -> c) -> b -> c) -> [b -> c] -> [b] -> [c]
[] `zipWith` _ = []
_ `zipWith` [] = []
(x : xs) `zipWith` (y : ys) = x y : xs `zipWith` ys
-}

infixl 1 >$<

(>$<) :: (a -> b) -> [a] -> [b]
f >$< as = map f as

infixl 0 >*<

(>*<) :: [a -> b] -> [a] -> [b]
funcs >*< as = zipWith id funcs as

x1s :: [Integer]
x1s = [1, 2, 3]

x2s :: [Integer]
x2s = [4, 5, 6]

x3s :: [Integer]
x3s = [7, 8, 9]

x4s :: [Integer]
x4s = [10, 11, 12]

example1 :: [Integer]
example1 = (\a b -> 2 * a + 3 * b) >$< x1s >*< x2s

example2 :: [Integer]
example2 = (\a b c d -> 2 * a + 3 * b + 5 * c - 4 * d) >$< x1s >*< x2s >*< x3s >*< x4s

-- Offtopic. My successful attempt to create nested functor transformation (preserve only 'first layer' structure)
nestedFunctorTransformation ::
  (Applicative f1, Applicative f2, Applicative f3) =>
  f1 (f2 a -> f3 b) ->
  f1 (f2 a) ->
  f1 (f3 b)
nestedFunctorTransformation af ff = af <*> ff

nestedListToLength :: Maybe [a] -> Maybe (Identity Int)
nestedListToLength = nestedFunctorTransformation (Just $ \xs -> Identity $ length xs)

nestedMaybeLength :: Maybe (Identity Int)
nestedMaybeLength = nestedListToLength $ Just [1, 2, 3] -- Just (Identity 3)

{-
instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = e
  Right f <*> r = fmap f r
-}

rightProduct :: Either e Integer
rightProduct = (*) <$> Right 7 <*> Right 4 -- Right 28

erroredCalculation :: Either String Integer
erroredCalculation = (*) <$> Left "Error!" <*> Right 8 -- Left "Error!"

erroredAnotherEx :: Bool -> Either String Integer
erroredAnotherEx shouldError = do
  f <- Right (*)
  partially <- if shouldError then Left "Error!" else Right $ f 4
  return $ partially 10

{-
instance (Monoid a) => Applicative ((,) a) where
  pure x = (mempty, x)
  (a1, func) <*> (a2, x) = (a1 <> a2, func x)
-}

applicativePairWithComments :: (String, Integer)
applicativePairWithComments = ("Answer to (+) ", (*)) <*> ("5 ", 5) <*> ("6 ", 6) -- ("Answer to (+) 5 6 ",30)

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' [] = ("1.0", 1)
divideList' (x : xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs

{-
instance Applicative ((->) e) where
  pure x _ = x -- impl 1
  -- pure x = \_ -> x -- impl 2
  -- pure = const -- impl 3

  -- (S combinator, generalized version of application)
  -- indeed: S x y z = x z (y z)
  (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
  eab <*> ea = \e -> eab e (ea e)
-}

arrowFuncApplicativeExample1 :: Integer
arrowFuncApplicativeExample1 = (*) <*> (* 3) $ 5 -- 5 * (5 * 3) = 75

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}

newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  fmap f (Arr2 a) = Arr2 $ \e1 e2 -> f $ a e1 e2

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  fmap f (Arr3 a) = Arr3 $ \e1 e2 e3 -> f $ a e1 e2 e3

instance Applicative (Arr2 e1 e2) where
  pure :: a -> Arr2 e1 e2 a
  pure x = Arr2 $ \_ _ -> x
  (<*>) :: Arr2 e1 e2 (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  (<*>) (Arr2 morphism) (Arr2 a) = Arr2 $ \e1 e2 -> morphism e1 e2 (a e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure :: a -> Arr3 e1 e2 e3 a
  pure x = Arr3 $ \_ _ _ -> x
  (<*>) :: Arr3 e1 e2 e3 (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  (<*>) (Arr3 morphism) (Arr3 a) = Arr3 $ \e1 e2 e3 -> morphism e1 e2 e3 (a e1 e2 e3)

-- S combinator: S x y z = x z (y z)
_m15 :: Integer
_m15 = getArr3 (Arr3 (\x y z w -> x + y + z - w) <*> Arr3 (\x y z -> x * y * z)) 2 3 4

example10 :: (Sum Integer, [(Integer, Integer)])
-- example10 = zip <$> (Sum 5, [1, 2, 3]) <*> (Sum 4, [5, 6])
-- example10 = (Sum 5, zip [1, 2, 3]) <*> (Sum 4, [5, 6])
example10 = pure zip <*> (Sum 5, [1, 2, 3]) <*> (Sum 4, [5, 6])

example11 :: [(Char, Char)]
example11 = (,) <$> "dog" <*> "cat"

example12 :: [(Integer, Integer)]
example12 = zip <*> tail $ [1, 2, 3] -- [(1, 2), (2, 3)]

example13 :: [(Integer, String)] -> Maybe String
-- example13 xs = (++) <$> lookup 3 xs <*> lookup 5 xs
example13 xs = pure (++) <*> lookup 3 xs <*> lookup 5 xs

example21 :: Maybe String
example21 = Just 12 *> Just "String" -- Just "String"

example22 :: Maybe Integer
example22 = Just 12 <* Just "String" -- Just 12

example23 :: [Integer]
-- like: concatMap (\_ -> rhsContainer) lhsContainer
example23 = [1, 2, 3] *> [4, 5] -- [4, 5, 4, 5, 4, 5] -- somewhat concatMap to values of the second container

example24 :: [Integer]
example24 = [1, 2] *> [4, 5, 6] -- [1, 1, 1, 2, 2, 2] -- (length lhs * length rhs)

example25 :: [Integer]
example25 = [1, 2] <* [4, 5, 6] -- [4, 5, 6, 4, 5, 6] -- (length lhs * length rhs)

example26 :: [(,) Integer Integer]
example26 = (,) <$> [1, 2] <*> [4, 5, 6] -- [(1,4), (1,5), (1,6), (2,4), (2,5), (2,6)]

example26IsActuallyAZip :: [(Integer, Integer)]
example26IsActuallyAZip = zip example25 example24

example27OnlyStructureMatters :: [Integer]
example27OnlyStructureMatters = [1, 2, 3] <* "abc" -- [1, 1, 1, 2, 2, 2, 3, 3, 3]

(<*!) :: (Applicative f) => f a -> f b -> f a
u <*! v = const <$> u <*> v

(!*>) :: (Applicative f) => f a -> f b -> f b
u !*> v = (\_ x -> x) <$> u <*> v

{-
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = f <$> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
-- Basically saying, it should change the order of calculations but not the order of effects.

(<**>) = flip (<*>) -- effects didn't swap (e.g. the same as `(,) <$> [1, 2, 3] <*> [4, 5]`)
>> ghci> flip (<*>) [4 :: Integer, 5] $ (,) <$> [1 :: Integer, 2, 3]
>> [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

(<**>) = liftA2 $ flip ($) -- effects swapped (e.g. the same as `flip (,) <$> [4, 5] <*> [1, 2, 3]`)
>> ghci> (liftA2 $ flip ($)) [4 :: Integer, 5] $ (,) <$> [1, 2, 3]
>> [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]
-}

infixl 4 <*?>

(<*?>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b. Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op =
  let (<??>) = op
      infixl 4 <??>
   in Just 5 <??> Just (+ 2) -- place for counterexample (no)

exprList :: (forall a b. [a] -> [a -> b] -> [b]) -> [Int]
exprList op =
  let (<??>) = op
      infixl 4 <??>
   in [5, 7] <??> [(+ 1), (+ 10)] -- place for counterexample (yes)

exprZipList :: (forall a b. ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op =
  let (<??>) = op
      infixl 4 <??>
   in ZipList [1, 2] <??> ZipList [(+ 3), (+ 4)] -- place for counterexample (no)

exprEither :: (forall a b. Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op =
  let (<??>) = op
      infixl 4 <??>
   in Left "AA" <??> Left "BB" -- place for counterexample (yes)

exprPair :: (forall a b. (String, a) -> (String, a -> b) -> (String, b)) -> (String, Int)
exprPair op =
  let (<??>) = op
      infixl 4 <??>
   in ("AA", 3) <??> ("D", (+ 1)) -- place for counterexample (yes)

exprEnv :: (forall a b. (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op =
  let (<??>) = op
      infixl 4 <??>
   in length <??> (\_ -> (+ 5)) -- place for counterexample (no)
