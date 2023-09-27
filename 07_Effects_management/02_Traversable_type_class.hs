module Demo () where

import Data.Foldable (asum, sequenceA_)

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

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

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ ini Nil = ini
  foldr f ini (Branch left x right) = x `f` foldr f (foldr f ini right) left -- preorder

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Nil = Nil
  fmap f (Branch left x right) = Branch (fmap f left) (f x) (fmap f right)

{-
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr (<>) mempty -- (<>) from the Semigroup type class

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty -- (<|>) from the Alternative type class
-}

-- Just 4, if preorder
example1 :: Maybe Integer
example1 = asum testTreeMaybied
  where
    testTreeMaybied :: Tree (Maybe Integer)
    testTreeMaybied = fmap Just testTree

-- Nothing, always.
example2 :: Maybe Integer
example2 = asum testTreeMaybied
  where
    testTreeMaybied :: Tree (Maybe Integer)
    testTreeMaybied = fmap (const Nothing) testTree

{-
sequenceA :: (Foldable t, Applicative f) => t (f a) -> f (t a)

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
-- it should perform application, ignore the value but promote the effect (context, structure)
-- (pure ()) will be used in the end of the fold (x1 *> (x2 *> (x3 *> pure ()))) -->> x1 *> x2 *> x3 *> pure ()
-- (*>) :: f a -> f b -> f b -- monomorphised: :: f a -> f () -> f ()
sequenceA_ = foldr (*>) (pure ())
-}

example3 :: IO ()
-- example3 = sequenceA_ $ fmap print testTree
example3 = sequenceA_ $ fmap (putStrLn . show) testTree

-- Just ()
example4 :: Maybe ()
example4 = sequenceA_ $ fmap Just testTree

-- Nothing
example5 :: Maybe ()
example5 = sequenceA_ $ fmap (const Nothing) testTree

-- Nothing, because one of calculations was erroneous
example6 :: Maybe ()
example6 = sequenceA_ [Just 1, Nothing, Just 2]

-- Left "Error occured!"
example7 :: Either String ()
example7 = sequenceA_ [Right 1, Right 2, Left "Error occured!", Right 4]

-- Right ()
example8 :: Either String ()
example8 = sequenceA_ [Right 1, Right 2, Right 4]

-- Left "1 was captured!"
example9 :: Either String ()
example9 = sequenceA_ $ fmap (\x -> if x == 1 then Left "1 was captured!" else Right x) testTree

-- Right ()
example10 :: Either String ()
example10 = sequenceA_ $ fmap (\x -> if x == 100 then Left "100 was captured!" else Right x) testTree

-- ("ABCDkek", ())
example11 :: (,) String ()
example11 = sequenceA_ [("AB", 1), ("CD", 2), pure 12, (^ 2) <$> ("kek", 4)]

-- list of twelve units (e.g. length example12 === 12)
example12 :: [] ()
example12 = sequenceA_ [replicate 4 undefined, replicate 3 undefined]

{-
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = sequenceA_ . map f . toList -- one of possible implementations (poor implementation)
traverse_ f = foldr ((*>) . f) (pure ()) -- more accurate implementation (genius implementation)
-}

{-
-- Notice the similarities between fold/foldMap and sequenceA_/traverse_!
-- Namely:
-- 1. (<>) <-> (*>)
-- 2. (<>) . f <-> (*>) . f
-- 3. mempty <-> pure () -- in sequenceA_/traverse_ we aren't interested in value, only in effects
--    -- whilst in fold/foldMap we're interested in the final mappended result

fold :: Monoid m => t m -> m
fold = foldr (<>) mempty

foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap f = foldr ((<>) . f) mempty

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())
-}

sequenceA2List :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2List = foldr (\x acc -> (:) <$> x <*> acc) (pure [])

-- ("HelloWorld", [1, 2])
example13 :: (String, [Integer])
example13 = sequenceA2List [("Hello", 1), ("World", 2)]

{-
sequenceA :: (Foldable t, Applicative f) => t (f a) -> f (t a)
-}

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr combine (pure [])
  where
    combine a fAcc = (:) <$> f a <*> fAcc

{-
class (Functor t, Foldable t) => Traversable t where
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse g = sequenceA . fmap g
  ....
-}

{-
instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse g (Just a) = Just <$> g a
-}

{-
instance Functor ((,) s) where
  fmap :: (a -> b) -> (,) s a -> (,) s b
  fmap f (x, y) = (x, f y)

instance Traversable ((,) s) where
  traverse :: Applicative f => (a -> f b) -> (,) s a -> f ((,) s b)
  traverse g (x, y) = (x,) <$> g y
-}

-- TASK BEGIN
data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr a0 a1 a2) = Tr (f a0) (f a1) (f a2)

instance Foldable Triple where
  foldr :: (a -> b -> b) -> b -> Triple a -> b
  foldr f ini (Tr a0 a1 a2) = foldr f ini [a0, a1, a2]

instance Traversable Triple where
  traverse :: (Applicative f) => (a -> f b) -> Triple a -> f (Triple b)
  traverse f (Tr a0 a1 a2) = Tr <$> f a0 <*> f a1 <*> f a2

-- TASK END

-- TASK BEGIN
data Result a = Ok a | Error String deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error err) = Error err
  fmap f (Ok a) = Ok $ f a

instance Foldable Result where
  foldr :: (a -> b -> b) -> b -> Result a -> b
  foldr _ ini (Error _) = ini
  foldr f ini (Ok a) = a `f` ini

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error err) = pure $ Error err
  traverse f (Ok a) = Ok <$> f a

-- TASK END

{-
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Foldable [] where
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ ini [] = ini
  foldr f ini (x : xs) = x `f` foldr f ini xs

instance Traversable [] where
  traverse :: (Applicative f) => (a -> f b) -> [a] -> f [b]
  traverse _ [] = pure []
  traverse g (x : xs) = (:) <$> g x <*> traverse g xs
-}

-- TASK BEGIN
instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Nil = pure Nil
  traverse g (Branch left x right) = Branch <$> traverse g left <*> g x <*> traverse g right

-- TASK END

{-
Useful comment from "Andrey Zavarin" https://stepik.org/users/300221583 :
>> Пришёл к такой интерпретации семантики Traversable, может кому будет интересно.
>>
>> Элементы типов, являющихся аппликативными функторами, как и в более общем случае монад,
>> естественно интерпретировать как описание некоторых вычислений с эффектами.
>> Представим теперь, что у нас есть некоторая структура данных (дерево, список, etc),
>> хранящая совокупность описаний вычислений с эффектами.
>> Давайте в том или ином смысле "пройдёмся" по этой структуре данных и "выполним" вычисления, хранящиеся в каждой вершине.
>> В итоге у нас получится та же структура данных, в вершинах которой хранятся "чистые" результаты этих вычислений,
>> а также какой-то накопленный, совокупный эффект всего того, что мы сделали. На уровне типов это и записывается,
>> как t (f a) -> f (t a) для Traversable t и Applicative f, то есть это в точности семантика функции sequenceA.
>>
>> В случае императивных языков эти эффекты -- это практически всегда изменение глобального состояния или взаимодействие с внешним миром,
>> а вот в Хаскеле точно так же можно, например, моделировать недетерминированные вычисления.
>>
>> Кстати говоря, видно, что полученный в итоге эффект зависит от порядка "обхода" структуры -- подозреваю, что в следующем разделе об этом пойдет речь.
-}

-- TASK BEGIN
infixr 9 |.|

newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor ((|.|) f g) where
  fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
  fmap f (Cmps c) = Cmps $ (f <$>) <$> c

instance (Traversable f, Traversable g) => Foldable ((|.|) f g) where
  foldr :: (a -> b -> b) -> b -> (|.|) f g a -> b
  foldr f ini (Cmps c) = foldr (flip (foldr f)) ini c

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
  traverse :: (Applicative l) => (a -> l b) -> (|.|) f g a -> l ((|.|) f g b)
  traverse liftMap (Cmps c) = Cmps <$> traverse (traverse liftMap) c

-- TASK END
