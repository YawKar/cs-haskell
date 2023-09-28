module Demo () where

import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Functor.Compose (Compose (Compose))
import Data.Traversable (fmapDefault, foldMapDefault)

{-
1st Identity law:

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

traverse Identity === Identity -- here Identity is a container

newtype Identity a = Identity {runIdentity :: a} -- it is a member of Monad, Functor and Applicative type classes

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

:t Identity is `a -> Identity a` and therefore can be used as the first argument to `traverse`
-- (a -> f b) === a -> Identity a (and thus, `f === Identity` and `a === b`)
-- btw, the whole signature after substitution: (a -> Identity a) -> t a -> Identity (t a)
-- and the law rewritten in types: t a -> Identity (t a) === a -> Identity a (t a === a)
-- therefore, law is correct
-}

-- True
example1 :: Bool
example1 = traverse Identity [1, 2, 3] == Identity [1, 2, 3]

{-
2nd Composition law:

{-
  Remember composition law for functors:
  fmap (g1 . g2) === fmap g1 . fmap g2
-}

traverse (Compose . fmap g2 . g1) === Compose . fmap (traverse g2) . traverse g1

traverse (Compose . fmap g2 . g1) cont === Compose (fmap (traverse g2) (traverse g1 cont))

cont :: Traversable t => t a
g1 :: Applicative f1 => a -> f1 b -- notice here `f1 b` `b` type is the same as
g2 :: Applicative f2 => b -> f2 c -- here because we compose them

:t Compose :: f (g a) -> Compose f g a
-}

example2 :: Compose Maybe (Either a) [Char] -- inner: Maybe (Either a [Char])
example2 = Compose $ Just $ Right "Abcd"

example3 :: Maybe (Either a [Char])
example3 = fmap (traverse Right) (traverse Just "ABC")

{-
Guarantees that are provided by Traversable:
1. Every 'knot' of our structure will be visited exactly once
2. If we call traverse with `pure` from the Functor type class, it is guaranteed that we will get back our traversable
   container lifted inside the specified functor
   (e.g. `traverse Just cont` will get us `Just (typeof cont)`, N.B. `Just` is `Maybe`'s `pure`)
3. Traverse doesn't change the structure of traversable container (effects can be performed, structure cannot change)
   (N.B. actually, structure cannot be modified, but can be fully absorbed by some effect,
   e.g. Either/Maybe have Left/Nothing constructors that discard value as a whole)
-}

{- Back to the laws...
3rd Naturality law:

t . traverse g === traverse (t . g) -- applicative homomorphism can be taken out of traverse's first argument composition
where
  t :: (Applicative f, Applicative g) => f a -> g a -- this should preserve the structure

  -- what is structure preservation?
  t (pure x) = pure x
    -- first `pure x` is for `f a` and the second one is for some other applicative functor `g a`
  t (x <*> y) = t x <*> t y
    -- `(x <*> y)` is `f a`, whilst `t x <*> t y` is `g a`
    -- (it's like, perhaps, translate everything into applicative `g` and only then apply)
-}

{-
sequenceA laws (all can be expressed through `traverse` laws, because sequenceA = traverse id):
(1) sequenceA . fmap Identity === Identity                             -- Identity
(2) sequenceA . fmap Compose === Compose . fmap sequenceA . sequenceA  -- Composition
(3) t . sequenceA === sequenceA . fmap t                               -- Naturality
-}

{-
TASK BEGIN
sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA
(Applicative f, Applicative g, Traversable t) => t (f (g a)) -> Compose f g (t a)

what's the type of `fmap sequenceA`?
1. sequenceA :: (Foldable t, Applicative f) => t (f a) -> f (t a)
   Therefore, after substitution we get that return type of the rightmost `sequenceA` is `f (t (g a))`
2. fmap :: (a -> b) -> f a -> f b ++ :t sequenceA
   we get:
   fmap sequenceA :: f (t (g a)) -> f (g (t a))
TASK END
-}

-- TASK BEGIN
-- Container that can contain only odd number of elements
data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

instance Functor OddC where
  fmap :: (a -> b) -> OddC a -> OddC b
  fmap f (Un a) = Un $ f a
  fmap f (Bi a0 a1 rest) = Bi (f a0) (f a1) $ fmap f rest

instance Foldable OddC where
  foldr :: (a -> b -> b) -> b -> OddC a -> b
  foldr f ini (Un a) = a `f` ini
  foldr f ini (Bi a0 a1 rest) = f a0 $ f a1 $ foldr f ini rest

instance Traversable OddC where
  traverse :: (Applicative f) => (a -> f b) -> OddC a -> f (OddC b)
  traverse f (Un a) = Un <$> f a
  traverse f (Bi a0 a1 rest) = Bi <$> f a0 <*> f a1 <*> traverse f rest

-- TASK END

newtype Temperature _system = Temperature Double deriving (Num, Show) -- with use of `GeneralizedNewtypeDeriving`

data Celsius -- has name but uninhabited (e.g. cannot be instantiated, type system constants)

data Fahrenheit -- has name but uninhabited (e.g. cannot be instantiated, type system constants)

data Kelvin -- has name but uninhabited (e.g. cannot be instantiated, type system constants)

comfortTemperature :: Temperature Celsius
comfortTemperature = 23

celsius2Fahrenheit :: Temperature Celsius -> Temperature Fahrenheit
celsius2Fahrenheit (Temperature c) = Temperature (1.8 * c + 32)

kelvin2Celsius :: Temperature Kelvin -> Temperature Celsius
kelvin2Celsius (Temperature k) = Temperature (-273.15 + k)

example4 :: Temperature Celsius
example4 = comfortTemperature - comfortTemperature

example5 :: Temperature Fahrenheit
-- cannot subtract them without convertion
example5 = celsius2Fahrenheit comfortTemperature - celsius2Fahrenheit comfortTemperature

newtype Const c a = Const {getConst :: c} deriving (Eq, Show)

instance Functor (Const c) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap f (Const c) = Const c

instance Foldable (Const c) where
  foldMap :: (Monoid m) => (a -> m) -> Const c a -> m
  foldMap _ _ = mempty

instance (Monoid c) => Applicative (Const c) where
  pure :: a -> Const c a
  pure _ = Const mempty

  (<*>) :: Const c (a -> b) -> Const c a -> Const c b
  Const c1 <*> Const c2 = Const $ c1 <> c2

instance Traversable (Const c) where
  traverse :: (Applicative f) => (a -> f b) -> Const c a -> f (Const c b)
  -- ahaha, we just ignore function and lift the same const up to the target functor
  traverse _ (Const c) = pure $ Const c

example6 :: Const Char (*)
example6 = Const 'z'

data OutAddr

data InAddr

example7 :: Const String OutAddr
example7 = Const @String @OutAddr "shdf194348sfl94bnk"

example8 :: Const String InAddr
example8 = Const "348sfl94bnkshdf194"

example9 :: Const [*] Char
example9 = pure 'a' -- Const {getConst = []}, because [] is a monoid with mempty = []

example10 :: Const String (*)
example10 = Const "AB" <*> Const "CD" -- Const {getConst = "ABCD"}

example11 :: Const String Integer
example11 = 4 <$ example10 -- Const {getConst = "ABCD"}

data Result a
  = Error
  | Ok a
  deriving (Eq, Show)

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ Error = pure Error
  traverse f (Ok v) = Ok <$> f v

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f = runIdentity . traverse (Identity . f)

instance Foldable Result where
  foldMap :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap f = getConst . traverse (Const . f)

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA :: (Applicative f) => Tree (f a) -> f (Tree a)
  sequenceA Nil = pure Nil
  -- sequenceA (Branch left x right) = liftA3 liftedBranch (sequenceA left) (sequenceA right) x
  --   where
  --     liftedBranch l r x = Branch l x r
  sequenceA (Branch left x right) = liftedBranch <$> sequenceA left <*> sequenceA right <*> x
    where
      liftedBranch l r x = Branch l x r

{-
class (Foldable t, Functor t) => Traversable t where
  sequenceA :: Applicative f => t (f a) -> f (t a)
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

  sequence :: Monad m => t (m a) -> m (t a)
  sequence = sequenceA

  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  mapM = traverse
-}
