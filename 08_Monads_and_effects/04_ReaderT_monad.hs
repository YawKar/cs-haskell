module Demo () where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Identity (Identity (runIdentity))

newtype Reader' r a = Reader' {runReader' :: r -> a} -- is actually `MReaderT r Identity a`

newtype ReaderT' r m a = ReaderT' {runReaderT' :: r -> m a}

reader' :: (Monad m) => (r -> a) -> ReaderT' r m a
reader' f = ReaderT' (return . f)

example1 :: ReaderT' (Integer, Integer) Identity String
example1 = reader' show

-- task begin

newtype Arr2T e1 e2 m a = Arr2T {getArr2T :: e1 -> e2 -> m a}

newtype Arr3T e1 e2 e3 m a = Arr3T {getArr3T :: e1 -> e2 -> e3 -> m a}

arr2 :: (Monad m) => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 op = Arr2T $ \e1 e2 -> return (op e1 e2)

arr3 :: (Monad m) => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 op = Arr3T $ \e1 e2 e3 -> return (op e1 e2 e3)

-- Just 42
example2 :: Maybe Integer
example2 = (getArr2T $ arr2 (+)) 33 9 -- takes `Maybe` from the context

-- Right 120
example3 :: Either String Integer
example3 = (getArr3T $ arr3 foldr) (*) 1 [1 .. 5]

-- ([], [1,2,3,4,5,6,7,8,9,10])
example4 :: ([Integer], [Integer])
example4 = runIdentity $ (getArr2T $ arr2 span) (> 3) [1 .. 10]

-- task end

instance Functor (Reader' r) where
  fmap :: (a -> b) -> Reader' r a -> Reader' r b
  fmap f (Reader' r) = Reader' $ f . r

instance (Functor m) => Functor (ReaderT' r m) where
  fmap :: (a -> b) -> ReaderT' r m a -> ReaderT' r m b
  fmap f rdr = ReaderT' (fmap f . runReaderT' rdr)

-- task begin
instance (Functor m) => Functor (Arr2T e1 e2 m) where
  fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  fmap f a = Arr2T $ \e1 e2 -> f <$> getArr2T a e1 e2

instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where
  fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  fmap f a = Arr3T $ \e1 e2 e3 -> f <$> getArr3T a e1 e2 e3

-- task end

instance Applicative (Reader' r) where
  pure :: a -> Reader' r a
  pure = Reader' . const

  (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
  rab <*> ra = Reader' $ \r -> runReader' rab r $ runReader' ra r

instance (Applicative m) => Applicative (ReaderT' r m) where
  pure :: a -> ReaderT' r m a
  pure = ReaderT' . const . pure

  (<*>) :: ReaderT' r m (a -> b) -> ReaderT' r m a -> ReaderT' r m b
  -- rmab <*> rma = ReaderT' $ \r -> runReaderT' rmab r <*> runReaderT' rma r
  rmab <*> rma = ReaderT' $ liftA2 (<*>) (runReaderT' rmab) (runReaderT' rma) -- lift to `(->) r` applicative!

example5 :: (Applicative f0, Applicative f1) => f0 (f1 (a -> b)) -> f0 (f1 a) -> f0 (f1 b)
example5 = liftA2 (<*>) -- composition of applicatives

-- task begin

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where
  pure :: a -> Arr2T e1 e2 m a
  pure x = Arr2T $ \_ _ -> pure x

  (<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
  aab <*> aa = Arr2T $ \e1 e2 -> getArr2T aab e1 e2 <*> getArr2T aa e1 e2

instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
  pure :: a -> Arr3T e1 e2 e3 m a
  pure x = Arr3T $ \_ _ _ -> pure x

  (<*>) :: Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
  aab <*> aa = Arr3T $ \e1 e2 e3 -> getArr3T aab e1 e2 e3 <*> getArr3T aa e1 e2 e3

-- task end

instance Monad (Reader' r) where
  (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
  ra >>= k = Reader' $ \r -> runReader' (k $ runReader' ra r) r

instance (Monad m) => Monad (ReaderT' r m) where
  (>>=) :: ReaderT' r m a -> (a -> ReaderT' r m b) -> ReaderT' r m b
  -- rma >>= k = ReaderT' $ \r -> runReaderT' rma r >>= (\a -> runReaderT' (k a) r)
  rma >>= k = ReaderT' $ \r -> do
    a <- runReaderT' rma r -- `runReaderT'` returns `m a`, and `<-` binds `a`
    runReaderT' (k a) r -- same, but returns `m b`

asks' :: (Monad m) => (r -> a) -> ReaderT' r m a
asks' f = ReaderT' $ return . f

ask' :: (Monad m) => ReaderT' r m r
ask' = asks' id

local' :: (Monad m) => (r -> r') -> ReaderT' r' m a -> ReaderT' r m a
local' rr' rdr' = ReaderT' $ runReaderT' rdr' . rr'

-- [43, 8, 15]
example6 :: [Integer]
example6 = runReaderT' (do x <- rdr; return (succ x)) 7
  where
    rdr :: ReaderT' Integer [] Integer
    rdr = ReaderT' $ \e -> [42, e, e * 2]

-- task begin

instance (Monad m) => Monad (Arr2T e1 e2 m) where
  (>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
  -- a >>= k = Arr2T $ \e1 e2 -> getArr2T a e1 e2 >>= (\a -> getArr2T (k a) e1 e2)
  ar >>= k = Arr2T $ \e1 e2 -> do
    a <- getArr2T ar e1 e2
    getArr2T (k a) e1 e2

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
  (>>=) :: Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3 m b) -> Arr3T e1 e2 e3 m b
  -- a >>= k = Arr3T $ \e1 e2 e3 -> getArr3T a e1 e2 e3 >>= (\a -> getArr3T (k a) e1 e2 e3)
  ar >>= k = Arr3T $ \e1 e2 e3 -> do
    a <- getArr3T ar e1 e2 e3
    getArr3T (k a) e1 e2 e3

-- task end

-- task begin

instance (MonadFail m) => MonadFail (Arr3T e1 e2 e3 m) where
  fail :: String -> Arr3T e1 e2 e3 m a
  fail s = Arr3T $ \_ _ _ -> fail s

-- task end

class MonadTrans' t where
  lift' :: (Monad m) => m a -> t m a

{-
MonadTrans laws:
(1) lift . return === return -- `a -> t m a` the same as `return` of out transformer
(2) lift (m >>= k) = lift m >>= (lift . k) -- `(lift . k)`, because `k :: a -> m b` and we need to lift it to transformer
  -- MonadTrans preserves inner monad structure
-}

instance MonadTrans' (ReaderT' r) where
  lift' :: (Monad m) => m a -> ReaderT' r m a
  lift' ma = ReaderT' $ const ma

-- [6,8]
example7 :: [Integer]
example7 = runReaderT' (do env <- ask'; f <- lift' [pred, succ]; return (f env)) 7

-- task begin
instance MonadTrans' (Arr2T e1 e2) where
  lift' :: (Monad m) => m a -> Arr2T e1 e2 m a
  lift' m = Arr2T $ \_ _ -> m

asks2 :: (Monad m) => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return $ f e1 e2

-- task end
