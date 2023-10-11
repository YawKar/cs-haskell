{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Demo () where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable

newtype CoroutineT m a = CoroutineT {runCoroutineT :: m (Either (CoroutineT m a) a)}

instance (Monad m) => Functor (CoroutineT m) where
  fmap :: (a -> b) -> CoroutineT m a -> CoroutineT m b
  fmap f cma = CoroutineT $ f' <$> runCoroutineT cma
    where
      f' (Left c) = Left (fmap f c)
      f' (Right a) = Right $ f a

instance (Monad m) => Applicative (CoroutineT m) where
  pure :: a -> CoroutineT m a
  pure = CoroutineT . return . Right

  (<*>) :: CoroutineT m (a -> b) -> CoroutineT m a -> CoroutineT m b
  (<*>) = ap

instance (Monad m) => Monad (CoroutineT m) where
  c >>= k = CoroutineT $ runCoroutineT c >>= either (return . Left . (>>= k)) (runCoroutineT . k)

instance MonadTrans CoroutineT where
  lift = CoroutineT . fmap Right

runCoroutines :: (Monad m) => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines c1 c2 = runCoroutineT c1 >>= either (runCoroutines c2) (const $ runCoroutine c2)

runCoroutine :: (Monad m) => CoroutineT m r -> m r
runCoroutine c = runCoroutineT c >>= either runCoroutine return

yield :: (Monad m) => CoroutineT m ()
yield = CoroutineT $ return $ Left $ return ()

example1 :: IO ()
example1 = do
  Left continuation <- runCoroutineT hello
  putStr "Wonderful "
  runCoroutine continuation
  where
    hello = do
      lift (putStr "Hello, ")
      yield
      lift (putStrLn "World!")

example2 :: IO ()
example2 = runCoroutines c1 c2
  where
    c1 = do
      lift (putStrLn "(1)Hello")
      yield
      lift (putStrLn "(1)World")
    c2 = do
      lift (putStrLn "(2)Oopsie")
      yield
      lift (putStrLn "(2)Woopsie")

instance (MonadWriter w m) => MonadWriter w (CoroutineT m) where
  writer :: (MonadWriter w m) => (a, w) -> CoroutineT m a
  writer = lift . writer
  listen :: (MonadWriter w m) => CoroutineT m a -> CoroutineT m (a, w)
  listen c = CoroutineT $ do
    ~(a, w) <- listen $ runCoroutineT c
    either (runCoroutineT . listen) (return . Right . (,w)) a
  pass :: (MonadWriter w m) => CoroutineT m (a, w -> w) -> CoroutineT m a
  pass c = CoroutineT $ do
    e1 <- runCoroutineT c
    case e1 of
      Left c2 -> runCoroutineT $ pass c2
      Right ~(a1, fw) -> do
        ~(_, w) <- listen $ runCoroutineT c
        tell $ fw w
        return $ Right a1

coroutine1 :: CoroutineT (Writer String) ()
coroutine1 = do
  tell "1"
  yield
  tell "2"

coroutine2 :: CoroutineT (Writer String) ()
coroutine2 = do
  tell "a"
  yield
  tell "b"

coroutine3, coroutine4 :: CoroutineT (Writer String) ()
coroutine3 = do
  tell "1"
  yield
  yield
  tell "2"
coroutine4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield

tests :: [Bool]
tests =
  fmap
    runner
    [ ("1ab2cd", (c3, c4)),
      ("1234512345", (c1, c1)),
      ("1ab2cd34", (c5, c4)),
      ("a1bc2d34", (c4, c5)),
      ("1a", (c2, c6)),
      ("1a", (c6, c2)),
      ("1s", (c2, c7)),
      ("s1", (c7, c2)),
      ("sa", (c6, c7)),
      ("sa", (c7, c6))
    ]
  where
    runner (ans, (c1, c2)) = ans == snd (runWriter $ runCoroutines c1 c2)

c1 :: CoroutineT (Writer String) ()
c1 = do
  tell "1"
  tell "2"
  tell "3"
  tell "4"
  tell "5"

c2 :: CoroutineT (Writer String) ()
c2 = do
  tell "1"

c3 :: CoroutineT (Writer String) ()
c3 = do
  tell "1"
  yield
  yield
  tell "2"

c4 :: CoroutineT (Writer String) ()
c4 = do
  tell "a"
  yield
  tell "b"
  yield
  tell "c"
  yield
  tell "d"
  yield

c5 :: CoroutineT (Writer String) ()
c5 = do
  tell "1"
  yield
  yield
  tell "2"
  yield
  yield
  yield
  tell "3"
  yield
  tell "4"

c6 :: CoroutineT (Writer String) ()
c6 = do
  yield
  tell "a"

c7 :: CoroutineT (Writer String) ()
c7 = do
  tell "s"
  yield
