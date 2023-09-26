module Demo () where

import Data.Foldable (fold)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid (Dual (Dual), Endo (Endo), First (First), Product (Product), Sum (Sum))

{-
class Foldable t where -- `t` has kind `* -> *`
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  -- there are some more functions in the Foldable type class... they'll be covered later (below)

-- foldr f ini [x1, x2, x3] === x1 `f` (x2 `f` (x3 `f` ini))
-- foldl f ini [x1, x2, x3] === ((ini `f` x1) `f` x2) `f` x3
-}

{-
instance Foldable [] where
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ ini [] = ini
  foldr f ini (x : xs) = x `f` foldr f ini xs

  foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl _ ini [] = ini
  foldl f ini (x : xs) = foldl f (ini `f` x) xs
-}

exampleFoldableList1 :: [Integer]
exampleFoldableList1 = a <> b -- [1, 2, 3, 3, 2, 1]
  where
    a = foldr (:) [] [1, 2, 3]
    b = foldl (flip (:)) [] [1, 2, 3]

{-
instance Foldable Maybe where
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ ini Nothing = ini
  foldr f ini (Just x) = x `f` ini

  foldl :: (b -> a -> b) -> b -> Maybe a -> b
  foldl _ ini Nothing = ini
  foldl f ini (Just x) = ini `f` x
-}

exampleFoldableMaybe1 :: Integer
exampleFoldableMaybe1 = foldr (*) 3 (Just 14) -- 42

exampleFoldableMaybe2 :: Integer
exampleFoldableMaybe2 = foldr (*) 3 Nothing -- 3

{-
instance Foldable (Either e) where -- `Left e` constructor plays role of `Nothing` in `Maybe a`
  foldr :: (a -> b -> b) -> b -> Either e a -> b
  foldr _ ini (Left _) = ini
  foldr f ini (Right x) = x `f` ini

  foldl :: (b -> a -> b) -> b -> Either e a -> b
  foldl _ ini (Left _) = ini
  foldl f ini (Right x) = ini `f` x
-}

exampleFoldableEither1 :: Integer
exampleFoldableEither1 = foldr (*) 3 (Right 14) -- 42

exampleFoldableEither2 :: Integer
exampleFoldableEither2 = foldr (*) 3 (Left "I am left:)") -- 3

{-
instance Foldable ((,) s) where
  foldr :: (a -> b -> b) -> b -> (s, a) -> b
  foldr f ini (_, x) = x `f` ini

  foldl :: (b -> a -> b) -> b -> (s, a) -> b
  foldl f ini (_, x) = ini `f` x
-}

exampleFoldablePair1 :: String
exampleFoldablePair1 = foldr (<>) " world" (14, "Hello,") -- "Hello, world"

-- TASK 1 BEGIN
data Triple a = Tr a a a deriving (Eq, Show)

instance Foldable Triple where
  foldr :: (a -> b -> b) -> b -> Triple a -> b
  foldr f ini (Tr a0 a1 a2) = foldr f ini [a0, a1, a2]

  foldl :: (b -> a -> b) -> b -> Triple a -> b
  foldl f ini (Tr a0 a1 a2) = foldl f ini [a0, a1, a2]

-- TASK 1 END

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ ini Nil = ini
  foldr f ini (Branch left x right) = foldr f (x `f` foldr f ini right) left

treeToListInOrder :: Tree a -> [a]
treeToListInOrder = foldr (:) []

-- [1, 2, 3, 4, 5]
exampleFoldableTreeInOrder :: [Integer]
exampleFoldableTreeInOrder = treeToListInOrder testTree

testTree :: Tree Integer
testTree =
  Branch
    ( Branch
        (Branch Nil 1 Nil)
        2
        (Branch Nil 3 Nil)
    )
    4
    (Branch Nil 5 Nil)

{-
    4
   / \
  2  5
 / \
1  3
-}

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Levelorder where
  foldr :: (a -> b -> b) -> b -> Levelorder a -> b
  foldr _ ini (LevelO Nil) = ini
  foldr f ini (LevelO t) = foldr f ini (bfs [t] [])
    where
      bfs :: [Tree a] -> [a] -> [a]
      bfs [] acc = reverse acc
      bfs roots acc = bfs nextRoots newAcc
        where
          nextRoots = concatMap flatTree roots
          newAcc = reverse curRootsValues ++ acc
          curRootsValues = mapMaybe toValue roots

      flatTree :: Tree a -> [Tree a]
      flatTree Nil = []
      flatTree (Branch left _ right) = [left, right]

      toValue :: Tree a -> Maybe a
      toValue Nil = Nothing
      toValue (Branch _ x _) = Just x

treeToListLevelOrder :: Levelorder a -> [a]
treeToListLevelOrder = foldr (:) []

-- [4, 2, 5, 1, 3]
exampleFoldableLevelOrder :: [Integer]
exampleFoldableLevelOrder = treeToListLevelOrder $ LevelO testTree

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)

instance Foldable Preorder where
  foldr :: (a -> b -> b) -> b -> Preorder a -> b
  foldr _ ini (PreO Nil) = ini
  foldr f ini (PreO (Branch left x right)) = x `f` foldr f (foldr f ini right') left'
    where
      left' = PreO left
      right' = PreO right

treeToListPreOrder :: Preorder a -> [a]
treeToListPreOrder = foldr (:) []

-- [4, 2, 1, 3, 5]
exampleFoldableTreePreOrder :: [Integer]
exampleFoldableTreePreOrder = treeToListPreOrder $ PreO testTree

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)

instance Foldable Postorder where
  foldr :: (a -> b -> b) -> b -> Postorder a -> b
  foldr _ ini (PostO Nil) = ini
  foldr f ini (PostO (Branch left x right)) = foldr f (foldr f (x `f` ini) right') left'
    where
      left' = PostO left
      right' = PostO right

treeToListPostOrder :: Postorder a -> [a]
treeToListPostOrder = foldr (:) []

-- [1, 3, 2, 5, 4]
exampleFoldableTreePostOrder :: [Integer]
exampleFoldableTreePostOrder = treeToListPostOrder $ PostO testTree

{-
-- more Foldable type class functions which operate on monoids

class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b

  fold :: Monoid m => t m -> m -- uses '<>' (`mappend`) from the Monoid type class implementation for t
  fold = foldr mappend mempty -- (or `mconcat`) foldr because it is better than foldl (bc foldr can work on infinite lists, for example)

  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap fam = foldr (mappend . fam) mempty
  .... and some additional functions which will be covered later (below)
-}

-- [1, 2, 3, 4, 5, 6, 7, 8]
exampleFoldableMonoidList1 :: [Integer]
exampleFoldableMonoidList1 = fold [[1 .. 5], [6 .. 8]]

-- Sum {getSum = 10}
exampleFoldableFoldMap1 :: Sum Integer
exampleFoldableFoldMap1 = foldMap Sum [1, 2, 3, 4]

-- Product {getProduct = 24}
exampleFoldableFoldMap2 :: Product Integer
exampleFoldableFoldMap2 = foldMap Product [1, 2, 3, 4]

-- Sum {getSum = 15}
exampleFoldableFoldMap3 :: Sum Integer
exampleFoldableFoldMap3 = foldMap Sum testTree

-- Product {getProduct = 120}
exampleFoldableFoldMap4 :: Product Integer
exampleFoldableFoldMap4 = foldMap Product testTree

-- "12345"
exampleFoldableFoldMap5 :: String
exampleFoldableFoldMap5 = foldMap show testTree

-- "42513"
exampleFoldableFoldMap6 :: String
exampleFoldableFoldMap6 = foldMap show (LevelO testTree)

-- "42135"
exampleFoldableFoldMap7 :: String
exampleFoldableFoldMap7 = foldMap show (PreO testTree)

-- "13254"
exampleFoldableFoldMap8 :: String
exampleFoldableFoldMap8 = foldMap show (PostO testTree)

{-
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m

  sum :: Num a => t a -> a
  sum = getSum . foldMap Sum

  product :: Num a => t a -> a
  product getProduct . foldMap Product

  null :: t a -> Bool
  null = foldr (\_ _ -> False) True

  toList :: t a -> [a]
  toList = foldr (:) []

  length :: t a -> Int
  length = foldr (\_ -> succ) 0

  maximum :: Ord a => t a -> a -- uses `Max(Max), getMax` semigroup from Data.Semigroup

  minimum :: Ord a => t a -> a -- uses `Min(Min), getMin` semigroup from Data.Semigroup

  elem :: Eq a => a -> t a -> Bool
  elem el = getAny . foldMap (Any . (== el)) -- uses `Any(Any), getAny` monoid from Data.Monoid

  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
-}

{-
-- Series of additional useful functions from Data.Foldable

and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool

concat :: Foldable t => t [a] -> [a]
concat = foldr (++) [] -- one of possible implementations
concat = fold -- can be implemented like that because [] is a monoid

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap = foldMap -- because [] is a monoid, (a -> m) -> t a -> m
-}

{-
newtype Endo a = Endo { appEndo :: a -> a }
-- Data.Monoid (Endo)

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id
-}

mkEndo :: (Foldable t) => t (a -> a) -> Endo a
mkEndo = foldr compose (Endo id)
  where
    compose f (Endo g) = Endo (f . g)

{-
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f ini cont = appEndo (foldMap (Endo . f) cont) ini
-}

{-
newtype Dual a = Dual { getDual :: a }

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty
-}

-- Dual {getDual = "Hello, world"}
exampleDualMonoid1 :: Dual String
exampleDualMonoid1 = Dual "world" <> Dual ", " <> Dual "Hello"

-- Dual {getDual = First {getFirst = Just 5}}
-- without Dual: First {getFirst = Just 3}
exampleDualMonoid2 :: Dual (First Integer)
exampleDualMonoid2 = foldMap (Dual . First) [Nothing, Just 3, Just 5, Nothing]

-- TASK BEGIN
infixr 9 |.|

newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  foldr :: (a -> b -> b) -> b -> (|.|) f g a -> b
  -- foldr f ini (Cmps fga) = foldr (flip (foldr f)) ini fga
  foldr f ini (Cmps fga) = foldr reduce_ga ini fga
    where
      reduce_ga ga ini' = foldr reduce_a ini' ga
      reduce_a a ini'' = a `f` ini''

-- TASK END
