module Demo () where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Control.Monad.Trans (MonadTrans, lift)

newtype Writer' w a = Writer' {runWriter' :: (a, w)}

newtype WriterT' w m a = WriterT' {runWriterT' :: m (a, w)}

writer' :: (Monad m) => (a, w) -> WriterT' w m a
writer' p = WriterT' $ return p

execWriter' :: Writer' w a -> w
execWriter' (Writer' (_, w)) = w

execWriterT' :: (Monad m) => WriterT' w m a -> m w
execWriterT' = fmap snd . runWriterT'

-- Functor instances
instance Functor (Writer' w) where
  fmap :: (a -> b) -> Writer' w a -> Writer' w b
  fmap f (Writer' (a, w)) = Writer' (f a, w)

instance (Functor m) => Functor (WriterT' w m) where
  fmap :: (a -> b) -> WriterT' w m a -> WriterT' w m b
  fmap f (WriterT' m_aw) = WriterT' $ f' <$> m_aw
    where
      f' ~(a, w) = (f a, w) -- lazy writer

-- Applicative instances
instance (Monoid w) => Applicative (Writer' w) where
  pure :: a -> Writer' w a
  pure = Writer' . (,mempty)

  (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
  wab <*> wa = Writer' (ab a, w0 <> w1)
    where
      ~(ab, w0) = runWriter' wab
      ~(a, w1) = runWriter' wa

instance (Monoid w, Applicative m) => Applicative (WriterT' w m) where
  pure :: a -> WriterT' w m a
  pure = WriterT' . pure . (,mempty)

  (<*>) :: WriterT' w m (a -> b) -> WriterT' w m a -> WriterT' w m b
  w_mab <*> w_ma = WriterT' $ liftA2 f mab ma
    where
      f ~(ab, w0) ~(a, w1) = (ab a, w0 <> w1)
      mab = runWriterT' w_mab
      ma = runWriterT' w_ma

-- [(9, "(^2) 3"), (16, "(^2) 4"), (25, "(^2) 5"), (27, "(^3) 3"), (64, "(^3) 4"), (125, "(^3) 5")]
example1 :: [(Integer, String)]
example1 = runWriterT' (WriterT' [((^ 2), "(^2)"), ((^ 3), "(^3)")] <*> WriterT' [(x, " " ++ show x) | x <- [3 .. 5]])

-- Monad instances

instance (Monoid w) => Monad (Writer' w) where
  (>>=) :: Writer' w a -> (a -> Writer' w b) -> Writer' w b
  wa >>= k = Writer' (b, w0 <> w1)
    where
      (a, w0) = runWriter' wa
      (b, w1) = runWriter' (k a)

instance (Monoid w, Monad m) => Monad (WriterT' w m) where
  (>>=) :: WriterT' w m a -> (a -> WriterT' w m b) -> WriterT' w m b
  -- wma >>= k = WriterT' $ runWriterT' wma >>= k'
  --   where
  --     k' (a, w0) = f' <$> m_bw
  --       where
  --         m_bw = runWriterT' $ k a
  --         f' (b, w1) = (b, w0 <> w1)
  wma >>= k = WriterT' $ do
    ~(a, w0) <- runWriterT' wma
    ~(b, w1) <- runWriterT' (k a)
    return (b, w0 <> w1)

instance (Monoid w, MonadFail m) => MonadFail (WriterT' w m) where
  fail :: String -> WriterT' w m a
  fail = WriterT' . fail -- fail inner monad

-- [(228, "Matches!")]
example2 :: [(Int, String)]
example2 = runWriterT' (do 1 <- WriterT' [(41, "FAILS (I will die!)"), (1, "Matches!")]; return 228)

-- task begin

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance Functor Logged where
  fmap f (Logged log v) = Logged log (f v)

instance Applicative Logged where
  pure = Logged ""
  Logged log0 ab <*> Logged log1 a = Logged (log0 ++ log1) $ ab a

instance (Functor m) => Functor (LoggT m) where
  fmap f l = LoggT $ (f <$>) <$> runLoggT l

instance (Applicative m) => Applicative (LoggT m) where
  pure = LoggT . pure . pure
  l_m_ab <*> l_m_a = LoggT $ (<*>) <$> runLoggT l_m_ab <*> runLoggT l_m_a

instance (Monad m) => Monad (LoggT m) where
  l_m_a >>= k = LoggT $ do
    Logged loga a <- runLoggT l_m_a
    Logged logb b <- runLoggT (k a)
    return (Logged (loga ++ logb) b)

instance (MonadFail m) => MonadFail (LoggT m) where
  fail = LoggT . fail

logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

-- task end

instance (Monoid w) => MonadTrans (WriterT' w) where
  lift :: (Monad m) => m a -> WriterT' w m a
  lift ma = WriterT' $ do
    a <- ma
    return (a, mempty)

-- [(0, "one"), (2, "one"), (9, "ten"), (11, "ten"), (19, "twenty"), (21, "twenty")]
example3 :: [(Integer, String)]
example3 = runWriterT' (do x <- wl3; f <- lift [pred, succ]; return (f x))
  where
    wl3 :: WriterT' String [] Integer
    wl3 = WriterT' [(1, "one"), (10, "ten"), (20, "twenty")]

tell' :: (Monad m) => w -> WriterT' w m ()
tell' = writer' . ((),)

-- [(0, "one told:)"), (2, "one told:)"), (9, "ten told:)"), (11, "ten told:)"), (19, "twenty told:)"), (21, "twenty told:)")]
example4 :: [(Integer, String)]
example4 = runWriterT' (do x <- wl3; f <- lift [pred, succ]; tell' " told:)"; return (f x))
  where
    wl3 :: WriterT' String [] Integer
    wl3 = WriterT' [(1, "one"), (10, "ten"), (20, "twenty")]

listen' :: (Monad m) => WriterT' w m a -> WriterT' w m (a, w)
listen' m = WriterT' $ do
  ~(a, w) <- runWriterT' m
  return ((a, w), w) -- inner `(a, w)` is a value, whilst outer `w` is a log

-- [(0, "oneone"), (2, "oneone"), (9, "tenten"), (11, "tenten"), (19, "twentytwenty"), (21, "twentytwenty")]
example5 :: [(Integer, String)]
example5 = runWriterT' (do (x, w) <- listen' wl3; f <- lift [pred, succ]; tell' w; return (f x))
  where
    wl3 :: WriterT' String [] Integer
    wl3 = WriterT' [(1, "one"), (10, "ten"), (20, "twenty")]

censor' :: (Monad m) => (w -> w) -> WriterT' w m a -> WriterT' w m a
censor' f m = WriterT' $ do
  ~(a, w) <- runWriterT' m
  return (a, f w)

-- [(0, "eno"), (2, "eno"), (9, "net"), (11, "net"), (19, "ytnewt"), (21, "ytnewt")]
example6 :: [(Integer, String)]
example6 = runWriterT' (do x <- censor' censF wl3; f <- lift [pred, succ]; return (f x))
  where
    wl3 :: WriterT' String [] Integer
    wl3 = WriterT' [(1, "one"), (10, "ten"), (20, "twenty")]
    censF :: String -> String
    censF = reverse

-- task begin
write2log :: (Monad m) => String -> LoggT m ()
write2log s = LoggT $ pure $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

-- task end

-- task begin

instance MonadTrans LoggT where
  lift :: (Monad m) => m a -> LoggT m a
  lift m = LoggT $ pure <$> m

-- task end
