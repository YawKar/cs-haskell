module Demo () where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Monoid (Sum (getSum))

newtype State' s a = State' {runState' :: s -> (a, s)}

newtype StateT' s m a = StateT' {runStateT' :: s -> m (a, s)} -- `s ->` is a part that `read(er)s`, `(a, s)` is a part that `write(r)s`

state' :: (Monad m) => (s -> (a, s)) -> StateT' s m a
state' f = StateT' $ return . f

-- Just (7, 8)
example1 :: Maybe (Integer, Integer)
-- example1 = runStateT' (StateT' $ \s -> Just (s + 3, s * 2)) 4
example1 = runStateT' (state' $ \s -> (s + 3, s * 2)) 4

-- [(5, (5, "+ 1;")), (6, (6, "+ 2;")), (7, (7, "+ 3;"))]
example2 :: [(Integer, (Integer, String))]
example2 =
  runStateT'
    ( StateT' $
        \(s, log) ->
          [ let ns = s + 1 in (ns, (ns, "+ 1;")),
            let ns = s + 2 in (ns, (ns, "+ 2;")), -- if using Writer: ((value, state), log) ~ StateT s(tate) (Writer l(og)) v(alue) ~ StateT s (Writer l) v
            let ns = s + 3 in (ns, (ns, "+ 3;"))
          ]
    )
    (4, "") -- [(value, state)]

-- task begin
evalStateT' :: (Monad m) => StateT' s m a -> s -> m a
evalStateT' st = fmap fst . runStateT' st

execStateT' :: (Monad m) => StateT' s m a -> s -> m s
execStateT' st = fmap snd . runStateT' st

-- task end

-- task begin
readerToStateT :: (Monad m) => ReaderT r m a -> StateT' r m a
readerToStateT r = StateT' $ \s -> (,s) <$> runReaderT r s

-- task end

-- Functor instances
instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f st = State' (updater . runState' st)
    where
      updater ~(a, s) = (f a, s)

instance (Functor m) => Functor (StateT' s m) where
  fmap :: (a -> b) -> StateT' s m a -> StateT' s m b
  fmap f st = StateT' (fmap updater . runStateT' st)
    where
      updater ~(a, s) = (f a, s)

-- [(36, 42), (49, 5), (64, 10)]
example3 :: [(Integer, Integer)]
example3 = runStateT' ((^ 2) <$> st) 5
  where
    st = StateT' $ \s -> [(s + 1, 42), (s + 2, s), (s + 3, 2 * s)]

-- Applicative instances
instance Applicative (State' s) where
  pure :: a -> State' s a
  pure = State' . (,)

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  sab <*> sa = State' newSt
    where
      newSt s = (ab a, s'')
        where
          (ab, s') = runState' sab s
          (a, s'') = runState' sa s'

instance (Monad m) => Applicative (StateT' s m) where
  pure :: a -> StateT' s m a
  pure a = StateT' $ \s -> pure (a, s)

  (<*>) :: StateT' s m (a -> b) -> StateT' s m a -> StateT' s m b
  s_m_'ab_s' <*> s_m_'a_s' = StateT' $ \s -> do
    -- using `Monad m` context
    ~(ab, s) <- runStateT' s_m_'ab_s' s
    ~(a, s) <- runStateT' s_m_'a_s' s
    return (ab a, s)

-- [(3, 36), (4, 36), (5, 36), (5, 36), (6, 36), (7, 36), (8, 36), (10, 36), (12, 36)]
example4 :: [(Integer, Integer)]
example4 = runStateT' stApplied 5
  where
    stApplied = st1 <*> st2
    st1 = StateT' $ \s -> [(f, s + 1) | f <- [pred, succ, (* 2)]]
    st2 = StateT' $ flip map [4 .. 6] . flip (,) . (^ 2)

-- [(7, 8), (7, 8), (7, 8), (13, 12), (13, 12), (13, 12), (40, 20), (40, 20), (40, 20)]
example5 :: [(Integer, Integer)]
example5 = runStateT' stApplied 5
  where
    stApplied = st1 <*> st2
    st1 = StateT' $ \s -> [(f, f s) | f <- [pred, succ, (* 2)]]
    st2 = StateT' $ \s -> [(ns, ns) | let ns = s * 2, _ <- [0 .. 2]]

-- Monad instances
instance Monad (State' s) where
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  sa >>= k = State' $ \s ->
    let ~(a, s') = runState' sa s
     in runState' (k a) s'

instance (Monad m) => Monad (StateT' s m) where
  (>>=) :: StateT' s m a -> (a -> StateT' s m b) -> StateT' s m b
  s_m_'a_s' >>= k = StateT' $ \s -> do
    ~(a, s) <- runStateT' s_m_'a_s' s
    runStateT' (k a) s

-- [(25, 7), (25, 7), (25, 7), (49, 7), (49, 7), (49, 7), (144, 7), (144, 7), (144, 7)]
example6 :: [(Integer, Integer)]
example6 = flip runStateT' 5 $ do
  f <- st1
  x <- f <$> st2
  return (x * x)
  where
    st1 = StateT' $ \s -> [(f, s) | f <- [pred, succ, (* 2)]]
    st2 = StateT' $ \s -> [(s + 1, s + 2) | _ <- [0 .. 2]]

-- MonadFail instances
instance (MonadFail m) => MonadFail (StateT' s m) where
  fail msg = StateT' $ \s -> (,s) <$> fail msg

-- MonadTrans instances
instance MonadTrans (StateT' s) where
  lift :: (Monad m) => m a -> StateT' s m a
  -- lift m = StateT' $ \s -> do
  --   a <- m
  --   return (a, s)
  lift m = StateT' $ \s -> (,s) <$> m

-- [(-1,5),(0,5),(1,5),(2,5),(1,5),(2,5),(3,5),(4,5)]
example7 :: [(Integer, Integer)]
example7 = flip runStateT' 5 $ do
  f <- lift [pred, succ]
  x <- lift [0 .. 3]
  return (f x)

get' :: (Monad m) => StateT' s m s
get' = state' $ \s -> (s, s)

put' :: (Monad m) => s -> StateT' s m ()
put' = state' . const . ((),)

modify' :: (Monad m) => (s -> s) -> StateT' s m ()
modify' f = state' $ \s -> ((), f s)

-- task with tree

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  deriving (Show)

tick :: StateT Integer (Writer (Sum Integer)) Integer
tick = do
  n <- get
  put (n + 1)
  return n

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)
  where
    go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
    go (Leaf x) = do
      lift $ tell 1
      Leaf <$> tick
    go (Fork left _ right) = do
      left' <- go left
      n <- tick
      right' <- go right
      return (Fork left' n right')

-- (Fork (Leaf 1) 2 (Leaf 3),2)
example8 :: (Tree Integer, Integer)
example8 = numberAndCount (Fork (Leaf ()) () (Leaf ()))
