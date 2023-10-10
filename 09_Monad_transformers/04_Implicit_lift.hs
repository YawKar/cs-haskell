{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Demo () where

import Control.Monad.Identity (Identity (Identity, runIdentity))
import Control.Monad.Reader (MonadReader (ask, local, reader), ReaderT)
import Control.Monad.State (MonadState (get, put, state), MonadTrans (lift), StateT (StateT, runStateT), gets)
import Control.Monad.Trans.Writer qualified as WriterT

infixl 7 ***

-- MultiParamTypeClasses - многопараметрические классы типов
-- FunctionalDependencies - функциональные зависимости
class Mult a b c | a b -> c where
  (***) :: a -> b -> c

instance Mult Int Int Int where
  (***) = (*)

instance Mult Double Double Double where
  (***) = (*)

instance Mult Double Integer Double where
  (***) d i = d * fromIntegral i

-- offtopic start
class Coercible' a b where
  coerce :: a -> b

data Person = Person
  { personName :: String,
    personSurname :: String,
    personAge :: Int
  }

deriving instance Show Person

data Teacher = Teacher
  { teacherName :: String,
    teacherSurname :: String,
    teacherAge :: Int,
    teacherSubject :: String
  }

deriving instance Show Teacher

instance Coercible' Teacher Person where
  coerce :: Teacher -> Person
  coerce (Teacher {teacherName, teacherSurname, teacherAge, teacherSubject}) =
    Person
      { personName = teacherName,
        personSurname = teacherSurname,
        personAge = teacherAge
      }

instance Coercible' Person Teacher where
  coerce :: Person -> Teacher
  coerce (Person {personName, personSurname, personAge}) =
    Teacher
      { teacherName = personName,
        teacherSurname = personSurname,
        teacherAge = personAge,
        teacherSubject = "N/A"
      }

-- offtopic end

-- task begin
class Functor' c e | c -> e where
  fmap' :: (e -> e) -> c -> c

instance Functor' [a] a where
  fmap' :: (a -> a) -> [a] -> [a]
  fmap' = map

instance Functor' (Maybe a) a where
  fmap' :: (a -> a) -> Maybe a -> Maybe a
  fmap' = fmap

-- task end

class (Monoid w, Monad m) => MonadWriter' w m | m -> w where
  writer' :: (a, w) -> m a
  tell' :: w -> m ()
  listen' :: m a -> m (a, w)

listens :: (MonadWriter' w m) => (w -> b) -> m a -> m (a, b)
listens fLog m = do
  ~(a, log) <- listen' m
  return (a, fLog log)

instance (Monoid w, Monad m) => MonadWriter' w (WriterT.WriterT w m) where
  writer' = WriterT.writer
  tell' = WriterT.tell
  listen' = WriterT.listen

instance (MonadWriter' w m) => MonadWriter' w (StateT s m) where
  writer' = lift . writer'
  tell' = lift . tell'
  listen' m = StateT $ \s -> do
    ~((a, s'), w) <- listen' (runStateT m s)
    return ((a, w), s')

-- task begin
data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance Functor Logged where
  fmap f (Logged log v) = Logged log (f v)

instance Applicative Logged where
  pure = Logged ""
  Logged log0 ab <*> Logged log1 a = Logged (log0 ++ log1) $ ab a

instance MonadTrans LoggT where
  -- lift :: (Monad m) => m a -> LoggT m a
  lift m = LoggT $ pure <$> m

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

write2log :: (Monad m) => String -> LoggT m ()
write2log s = LoggT $ pure $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

instance (MonadState s m) => MonadState s (LoggT m) where
  -- get = LoggT $ do pure <$> get
  -- get = lift get
  -- put = lift . put
  state = lift . state -- state is a minimal req

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

instance (MonadReader r m) => MonadReader r (LoggT m) where
  ask = lift ask
  local rr = LoggT . local rr . runLoggT
  reader ra = lift $ reader ra

class (Monad m) => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance (Monad m) => MonadLogg (LoggT m) where
  w2log = write2log
  logg = LoggT . return

instance (MonadLogg m) => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg = lift . logg

instance (MonadLogg m) => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg = lift . logg
