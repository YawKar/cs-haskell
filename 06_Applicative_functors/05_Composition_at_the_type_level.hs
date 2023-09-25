module Demo () where

import Control.Applicative (Applicative (liftA2))
import Data.Char (digitToInt)

infixr 9 |.|

-- :k (<.>) is `:: (k -> *) -> (k1 -> k) -> k1 -> *`
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
  deriving (Eq, Show)

exampleUse1 :: ([] |.| Maybe) Int
exampleUse1 = Cmps [Just 1]

exampleUse2 :: (Maybe |.| []) Char
exampleUse2 = Cmps $ Just "abcde"

exampleUse3 :: Maybe [Char]
exampleUse3 = getCmps (Cmps ((Just "abcdef" :: Maybe [Char]) :: Maybe ([] Char)) :: (Maybe |.| []) Char)

-- TASK 1 BEGIN
type A = ((,) Integer |.| (,) Char) Bool

type B t = ((,,) Bool (t -> t) |.| Either String) Int

type C = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (4, ('a', True))

b :: B t
b = Cmps (True, id, Left "sd")

bJustForTheSakeOfIt :: B t
bJustForTheSakeOfIt = Cmps (True, id, Right 4)

c :: C
c = Cmps f
  where
    f :: Bool -> Integer -> Integer
    f _ i = i

-- TASK 1 END

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
  fmap f (Cmps composition) = Cmps (fmap f <$> composition)

-- Cmps {getCmps = [Just 1,Just 2,Nothing,Just 3]}
exampleUseFmapForFunctors1 :: (|.|) [] Maybe Int
exampleUseFmapForFunctors1 = digitToInt <$> Cmps [Just '1', Just '2', Nothing, Just '3']

-- Cmps {getCmps = Just "bcde"}
exampleUseFmapForFunctors2 :: (|.|) Maybe [] Char
exampleUseFmapForFunctors2 = succ <$> Cmps (Just "abcd")

-- TASK 2 BEGIN
newtype Cmps3 f g h a = Cmps3 {getCmps3 :: f (g (h a))}
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap :: (a -> b) -> Cmps3 f g h a -> Cmps3 f g h b
  fmap f (Cmps3 composition) = Cmps3 $ ((f <$>) <$>) <$> composition

-- Cmps3 {getCmps3 = [[[1],[4,9,16],[25,36]],[],[[49,64],[81,100,121]]]}
exampleUseFmapForCmps3 :: Cmps3 [] [] [] Integer
exampleUseFmapForCmps3 = (^ 2) <$> Cmps3 [[[1], [2, 3, 4], [5, 6]], [], [[7, 8], [9, 10, 11]]]

-- TASK 2 END

{-
Remember functor laws:
(1) fmap id container === container

fmap id (Cmps x) -- def fmap (Cmps)
=== Cmps $ fmap (fmap id) x -- fmap (g)
=== Cmps $ fmap id x -- fmap (f)
=== Cmps $ x -- Q.E.D.
-}

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure :: a -> (|.|) f g a
  pure = Cmps . pure . pure

  (<*>) :: (|.|) f g (a -> b) -> (|.|) f g a -> (|.|) f g b
  -- cfgab <*> cfga = Cmps $ liftA2 (<*>) (getCmps cfgab) (getCmps cfga)
  -- Cmps fgab <*> Cmps fga = Cmps $ op <$> fgab <*> fga
  --   where
  --     op :: g (a -> b) -> g a -> g b
  --     op gab ga = gab <*> ga
  Cmps fgab <*> Cmps fga = Cmps $ (<*>) <$> fgab <*> fga

-- TASK 3 BEGIN
unCmps3 :: (Functor f) => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = (getCmps <$>) . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = ((getCmps <$>) . getCmps <$>) . getCmps

-- TASK 3 END
