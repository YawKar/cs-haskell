module Demo () where

import Control.Applicative ((<**>))

{-
($)   ::                     (a -> b)   ->   a ->   b
(<$>) :: Functor     f  =>   (a -> b)   -> f a -> f b
(<*>) :: Applicative f  => f (a -> b)   -> f a -> f b
(=<<) :: Monad       m  =>   (a -> m b) -> m a -> m b

(&)    ::                    a ->   (a -> b) ->   b -- Data.Function
(<&>)  :: Functor     f => f a ->   (a -> b) -> f b -- Control.Lens.Operators
(<**>) :: Applicative f => f a -> f (a -> b) -> f b -- Control.Applicative
(>>=)  :: Monad       m => m a -> (a -> m b) -> m b
-}

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- ("BC", (2, 3))
example1 :: (String, (Integer, Integer))
example1 = do x <- ("B", 2); y <- ("C", 3); return (x, y)

-- ("BC", (2, 3))
example2 :: (String, (Integer, Integer))
example2 = ("B", 2) >>= \x -> ("C", 3) >>= \y -> return (x, y)

-- ("BC", (2, 3))
example3 :: (String, (Integer, Integer))
example3 = (\x -> (\y -> return (x, y)) =<< ("C", 3)) =<< ("B", 2)

-- ("BC", (2, 3))
example4 :: (String, (Integer, Integer))
example4 = pure (,) <*> ("B", 2) <*> ("C", 3)

-- ("CB", (2, 3))
example5 :: (String, (Integer, Integer))
-- notice that the order of log (effect) is preserved (left-to-right) (not flipped order of effects)
example5 = ("C", 3) <**> (("B", 2) <**> pure (,))

{-
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
xs <**> fs = pure (&) <*> xs <*> fs
-}

(<***>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<***>) = flip (<*>)

-- ("BC", (2, 3))
example6 :: (String, (Integer, Integer))
-- notice that the order of log (effect) is NOT preserved (right-to-left) (flipped order of effects)
example6 = ("C", 3) <***> (("B", 2) <***> pure (,))

example7 :: [(Integer, Integer)]
example7 = (,) <$> [1 .. 3] <*> [1 .. 3] -- 9 pairs

example8 :: [(Integer, Integer)]
example8 = do a <- [1 .. 3]; b <- [1 .. 3]; return (a, b) -- 9 pairs (the same)

{- But what's the difference between applicative and monad? Answer: flexibility -}

example9 :: [(Integer, Integer)]
-- notice that range of the `b` variable is now lower-bounded by `a` instead of 1;
-- we can't do that using applicative functors, because the structure of the whole calculation
-- -- is `baked`;
-- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
example9 = do a <- [1 .. 3]; b <- [a .. 3]; return (a, b)

{-
In monadic calculations, previous value can affect consequent calculations.
-- (there's the Kleisli's arrow which can process previous value)
In applicative functors, it isn't possible.
-- (there's no function for value-processing in applicative <*>)
-}

funM :: (Num b, MonadFail m, Ord b) => m b -> m b
-- we can analyze the value and change the order of calculations!
funM mv = mv >>= \v -> if v > 0 then return (v ^ 2) else fail ""

funA :: (Applicative f, Num b) => f b -> f b
funA mv = (^ 2) <$> mv -- we cannot analyze the value! (as we can in monadic in the example above)

-- Just 100
example10 :: Maybe Integer
example10 = funM $ Just 10

-- Nothing
example11 :: Maybe Integer
example11 = funM $ Just 0

-- TASK BEGIN
newtype PrsE a = PrsE {runPrsE :: String -> Either String (a, String)}

instance Functor PrsE where
  fmap :: (a -> b) -> PrsE a -> PrsE b
  fmap f (PrsE prs) = PrsE newPrs
    where
      newPrs s = case prs s of
        Left err -> Left err
        Right (v, rest) -> Right (f v, rest)

instance Applicative PrsE where
  pure :: a -> PrsE a
  pure x = PrsE $ \s -> Right (x, s)

  (<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
  PrsE f <*> PrsE fa = PrsE newPrs
    where
      newPrs s = case f s of
        Left err -> Left err
        Right (transform, rest) ->
          case fa rest of
            Left err -> Left err
            Right (a, rest') -> Right (transform a, rest')

instance Monad PrsE where
  (>>=) :: PrsE a -> (a -> PrsE b) -> PrsE b
  PrsE pa >>= k = PrsE newPrs
    where
      newPrs s = case pa s of
        Left err -> Left err
        Right (a, rest) -> runPrsE (k a) rest

-- TASK END

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mab ma = do
  ab <- mab
  ab <$> ma

ap'' :: (Monad m) => m (a -> b) -> m a -> m b
ap'' mab ma = mab >>= \f -> ma >>= return . f

liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f xm = do
--   x <- xm
--   return (f x)
liftM f xm = f <$> xm

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
-- liftM2 f ma mb = do
--   a <- ma
--   b <- mb
--   return (f a b)
liftM2 f ma mb = f <$> ma <*> mb

data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a0) (Un a1) x = Bi a0 a1 x
concat3OC (Un a0) (Bi a1 a2 x) y = Bi a0 a1 $ concat3OC (Un a2) x y
concat3OC (Bi a0 a1 x) y z = Bi a0 a1 $ concat3OC x y z

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi a0 a1 x) = concat3OC a0 a1 $ concatOC x

instance Functor OddC where
  fmap :: (a -> b) -> OddC a -> OddC b
  fmap f (Un a) = Un $ f a
  fmap f (Bi a0 a1 x) = Bi (f a0) (f a1) $ fmap f x

instance Applicative OddC where
  pure :: a -> OddC a
  pure = Un

  (<*>) :: OddC (a -> b) -> OddC a -> OddC b
  Un ab <*> x = ab <$> x
  Bi ab0 ab1 x <*> y = concat3OC (ab0 <$> y) (ab1 <$> y) (x <*> y)

instance Monad OddC where
  (>>=) :: OddC a -> (a -> OddC b) -> OddC b
  Un a >>= k = k a
  Bi a0 a1 x >>= k = concat3OC (k a0) (k a1) (x >>= k)
