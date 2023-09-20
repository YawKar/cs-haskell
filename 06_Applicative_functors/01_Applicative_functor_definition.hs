module Demo () where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Maybe (fromMaybe)

{-
rewind some of container-types

newtype Identity a = Identity a

data Maybe a = Nothing | Just a

data (,) a b = (a, b)

data Either a b = Left a | Right b

data [] a = [] | a : [] a --recursive

data Tree a = Leaf | Branch (Tree a) a (Tree a) -- recursive
-}

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Identity where
  fmap g (Identity x) = Identity $ g x

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just $ g x

instance Functor [] where
  fmap = map
-}

{-
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap _ (Left e) = Left e
  fmap g (Right a) = Right $ g a

instance Functor ((,) s) where
  fmap :: (a -> b) -> (s, a) -> (s, b)
  fmap g (s, a) = (s, g a)

instance Functor ((->) e) where
  fmap :: (a -> b) -> (e -> a) -> (e -> b)
  fmap g f = \e -> g (f e)
  fmap = (.)
-}

{-
What is the type of the leftmost `<$>` in `succ <$> succ <$> "abc"` if `<$>` is defined as infixl 4?
(<$>) :: Functor f => (a -> b) -> f a -> f b

1. Rewrite it using ((->) e) as a functor
(<$>) :: (a -> b) -> (e -> a) -> e -> b

2. Substitute (a -> b) with monomorphized type of `succ`, which is `Char -> Char`
(<$>) :: (Char -> Char) -> (e -> Char) -> e -> Char

3. Consider substitution of `e` in `(e -> Char)` using second monomorphized `succ`
(<$>) :: (Char -> Char) -> (Char -> Char) -> Char -> Char

Answer: (Char -> Char) -> (Char -> Char) -> Char -> Char
-}

-- These types encapsulate calculations with 2 or 3 independent environments
newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}

newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  fmap g (Arr2 arr2) = Arr2 $ \e1 e2 -> g $ arr2 e1 e2

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  fmap f (Arr3 arr3) = Arr3 $ \e1 e2 e3 -> f $ arr3 e1 e2 e3

exampleArr2 :: Arr2 Int [a] Int
exampleArr2 = fmap length (Arr2 take)

exampleArr2Usage :: Int
exampleArr2Usage = getArr2 exampleArr2 8 [5 .. 30] -- 8

exampleArr3 :: Arr3 (a -> b -> c) [a] [b] Int
exampleArr3 = (^ 2) <$> length <$> Arr3 zipWith

exampleArr3Usage :: Int
exampleArr3Usage = getArr3 exampleArr3 (+) [0 .. 10] [11 .. 20]

{-
Functor laws:
(1) fmap id container = container
(2) ((fmap f) . (fmap g)) container = fmap (f . g) container
(2*) fmap f (fmap g container) = fmap (f . g) container
-}

-- Pointed f type class's just for educational purposes to demonstrate one of
-- the extensions that Applicative provides, namely, pure
class (Functor f) => Pointed f where
  pure :: a -> f a -- aka singleton, return, unit, point

instance Pointed Maybe where
  pure :: a -> Maybe a
  pure = Just

instance Pointed [] where
  pure :: a -> [a]
  pure x = [x]

instance Pointed (Either e) where
  pure :: a -> Either e a
  pure = Right

instance Pointed ((->) e) where
  pure :: a -> e -> a
  pure = const

instance (Monoid s) => Pointed ((,) s) where
  pure :: a -> (s, a)
  pure v = (mempty, v)

{-
Pointed type class law:
fmap g (pure x) = pure (g x)
-- actually, we cannot write such an implementation of `pure`
-- that won't satisfy aforementioned equation
-- (provided that the implementation of the fmap function is correct)
-}

-- Unsuccessfull attempt to create a binary functor mapping using only fmap
-- That's why Applicative was introduced in Haskell
-- binaryFunctorMappinng :: Functor f => (a -> b -> c) -> f a -> f b -> f c
-- binaryFunctorMappinng op fa fb =
--   fmap (\b -> fmap (\arrow -> (arrow b)) (fmap op fa)) fb

-- not really necessary, added it just to embrace its operatorness
infixl 9 <*!>

(<*!>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<*!>) = flip (<*>)

infixl 9 <$!>

(<$!>) :: (Functor f) => f a -> (a -> b) -> f b
(<$!>) = flip (<$>)

-- calculations perform from left to right
k :: Maybe ([Char], [Char])
k = Just "Hello world" <*!> Just length <*!> Just (* 2) <*!> Just show <*!> Just (splitAt 1)

k' :: Maybe ([Char], [Char])
k' = Just "Hello world" <$!> length <$!> (* 2) <$!> show <$!> splitAt 1

-- now examples where Applicative can be effective
appUsage1 :: Maybe (Integer, Integer)
appUsage1 = Just (,) <*> Just 1 <*> Just 2 -- Just (1, 2)

appUsage2 :: Maybe (Integer, Integer)
appUsage2 = Just flip <*> Just (,) <*> Just 2 <*> Just 1 -- Just (1, 2)

appUsageWithNothing1 :: Maybe (Integer, Integer)
appUsageWithNothing1 = Just (,) <*> Nothing <*> Nothing -- Nothing

-- type class for educational purposes
class (Functor f) => Apply f where
  (#<*>) :: f (a -> b) -> f a -> f b

instance Apply Maybe where
  (#<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing #<*> _ = Nothing
  _ #<*> Nothing = Nothing
  Just morphism #<*> b = fmap morphism b

myAppUsage1 :: Maybe (Integer, Integer)
myAppUsage1 = Just (,) #<*> Just 1 #<*> Just 2 -- Just (1, 2)

myAppUsage2 :: Maybe (Integer, Integer)
myAppUsage2 = Just flip #<*> Just (,) #<*> Just 2 #<*> Just 1 -- Just (1, 2)

myAppUsageWithNothing1 :: Maybe (Integer, Integer)
myAppUsageWithNothing1 = Just (,) #<*> Nothing #<*> Nothing -- Nothing

data DbConnection = DbConnection {host :: Host, port :: Port, driver :: Driver}
  deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

newtype Port = Port Integer deriving (Eq, Show)

newtype Driver = Driver String deriving (Eq, Show)

produceHost :: (Applicative f) => f Host
produceHost = Prelude.pure $ Host "127.0.0.1"

producePort :: (Applicative f) => f Port
producePort = Prelude.pure $ Port 5432

produceDriver :: (Applicative f) => f Driver
produceDriver = Prelude.pure $ Driver "pgdrv"

mkDbConnection :: (Applicative f) => f DbConnection
mkDbConnection = DbConnection <$> produceHost <*> producePort <*> produceDriver

kekich :: DbConnection
kekich = runIdentity mkDbConnection

instance Apply [] where
  -- note that actual implementation for lists is different from the one shown here
  -- btw, the actual implementation works like:
  -- [id, (*2), (*5)] <*> [2, 3] === [2, 3, 4, 6, 10, 15]
  -- (essentially, it's a concatMap: \funcs xs -> concatMap (\f -> map f xs) funcs)
  (#<*>) :: [a -> b] -> [a] -> [b]
  -- (g : gs) #<*> (x : xs) = g x : gs #<*> xs -- this implementation works like zipWith
  (#<*>) = zipWith id -- :t zipWith becomes ((a -> b) -> a -> b) -> [a -> b] -> [a] -> [b]

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  Just morphism <*> x = fmap morphism x
-}

{-
cont :: f a
g :: a -> b
fmap g cont :: f b !!!

pure g :: f (a -> b)
pure g <*> cont :: f b !!!
law of structure-preservation:
fmap g cont === pure g <*> cont
-}

{-
Applicative laws:
(1) Identity law
(1) pure id <*> v === v

(2) Homomorphism law
(2) pure g <*> pure x === pure (g x)
(2) types:
(2) g :: a -> b
(2) x :: a
(2) g x :: b
(2) <*> :: f (a -> b) -> f a -> f b

(3) Interchange law
(3) cont <*> pure x === pure ($ x) <*> cont
(3) types:
(3) cont :: f (a -> b)
(3) x :: a
(3) ($) :: (a -> b) -> a -> b
(3) ($ x) :: (a -> b) -> b
(3) pure ($ x) :: f ((a -> b) -> b)
(3) (<*>) :: f ((a -> b) -> b) -> f (a -> b) -> f b

(4) Composition law
(4) pure (.) <*> u <*> v <*> cont === u <*> (v <*> cont)
(4) types:
(4)   left hand side:
(4)     (.) :: (b -> c) -> (a -> b) -> a -> c
(4)     pure (.) :: f ((b -> c) -> (a -> b) -> a -> c)
(4)     u :: f (b -> c)
(4)     v :: f (a -> b)
(4)     cont :: f a
(4)     pure (.) <*> u :: f ((a -> b) -> a -> c)
(4)     pure (.) <*> u <*> v :: f (a -> c)
(4)     pure (.) <*> u <*> v <*> cont :: f c
(4)   right hand side:
(4)     v <*> cont :: f b
(4)     u <*> (v <*> cont) :: f c
-}

data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure :: a -> Triple a
  pure x = Tr x x x

  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  Tr f1 f2 f3 <*> Tr a b c = Tr (f1 a) (f2 b) (f3 c)
