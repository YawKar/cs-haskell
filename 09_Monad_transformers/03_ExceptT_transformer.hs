module Demo () where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (guard, msum)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (StateT (runStateT), modify, put)
import Control.Monad.Trans (MonadTrans (lift), liftIO)
import Control.Monad.Trans.Except (ExceptT, catchE, except, runExceptT, throwE)
import Control.Monad.Trans.Maybe
import Data.Char (isNumber, isPunctuation)
import Text.Read (readEither, readMaybe)

newtype Except' e a = Except' {runExcept' :: Either e a}

newtype ExceptT' e m a = ExceptT' {runExceptT' :: m (Either e a)}

throw' :: e -> Except' e a
throw' = Except' . Left

throwT' :: (Monad m) => e -> ExceptT' e m a
throwT' = ExceptT' . return . Left

exceptT' :: (Monad m) => Either e a -> ExceptT' e m a
exceptT' = ExceptT' . return

catchT' :: (Monad m) => ExceptT' e m a -> (e -> ExceptT' e' m a) -> ExceptT' e' m a
catchT' exc h = ExceptT' $ do
  ea <- runExceptT' exc
  either (runExceptT' . h) (return . Right) ea

-- Functor instances
instance Functor (Except' e) where
  fmap :: (a -> b) -> Except' e a -> Except' e b
  fmap f = Except' . fmap f . runExcept'

instance (Functor m) => Functor (ExceptT' e m) where
  fmap :: (a -> b) -> ExceptT' e m a -> ExceptT' e m b
  fmap f = ExceptT' . fmap (fmap f) . runExceptT'

-- Just (Right 74088)
example1 :: Maybe (Either String Integer)
example1 = runExceptT' $ fmap (^ 3) $ exceptT' $ Right 42

-- Applicative instances
instance Applicative (Except' e) where
  pure :: a -> Except' e a
  pure = Except' . Right

  (<*>) :: Except' e (a -> b) -> Except' e a -> Except' e b
  e_'e_ab' <*> e_'e_a' = Except' $ e_ab <*> e_a
    where
      e_ab = runExcept' e_'e_ab'
      e_a = runExcept' e_'e_a'

instance (Monad m) => Applicative (ExceptT' e m) where
  pure :: a -> ExceptT' e m a
  pure = ExceptT' . pure . Right

  (<*>) :: ExceptT' e m (a -> b) -> ExceptT' e m a -> ExceptT' e m b
  -- exc_m_'e_ab' <*> exc_m_'e_a' = ExceptT' $ liftA2 (<*>) m_'e_ab' m_'e_a' -- this impl. bad, bc it performs effects independently of a value
  --   where
  --     m_'e_ab' = runExceptT' exc_m_'e_ab'
  --     m_'e_a' = runExceptT' exc_m_'e_a'
  excmeab <*> excmea = ExceptT' $ do
    eab <- runExceptT' excmeab
    case eab of
      Left err -> return $ Left err -- notice that we aren't using the second applicative `excmea` and that is how it should work
      Right ab -> fmap ab <$> runExceptT' excmea

-- Monad instances
instance Monad (Except' e) where
  (>>=) :: Except' e a -> (a -> Except' e b) -> Except' e b
  exc_a >>= k = Except' $ case runExcept' exc_a of
    Left e -> Left e
    Right a -> runExcept' (k a)

instance (Monad m) => Monad (ExceptT' e m) where
  (>>=) :: ExceptT' e m a -> (a -> ExceptT' e m b) -> ExceptT' e m b
  exc >>= k = ExceptT' $ do
    ea <- runExceptT' exc
    case ea of
      Left e -> return $ Left e
      Right r -> runExceptT' $ k r

-- MonadFail instances
instance (MonadFail m) => MonadFail (ExceptT' e m) where
  fail :: String -> ExceptT' e m a
  fail = ExceptT' . fail

-- task begin
data Tile = Floor | Chasm | Snake
  deriving (Show)

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)

type GameMap = Point -> Tile

up :: Point -> Point
up (x, y) = (x - 1, y)

down :: Point -> Point
down (x, y) = (x + 1, y)

left :: Point -> Point
left (x, y) = (x, y - 1)

right :: Point -> Point
right (x, y) = (x, y + 1)

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves _ 0 startP = [Right startP]
moves gm steps startP = runExceptT' $ ExceptT' $ do
  f <- [up, down, left, right]
  let nextP = f startP
  case gm nextP of
    Floor -> moves gm (pred steps) nextP
    Chasm -> [Left Fallen]
    Snake -> [Left Poisoned]

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie dr gm steps startP = length $ filter matchDeathReason $ moves gm steps startP
  where
    matchDeathReason (Left dr') = dr' == dr
    matchDeathReason _ = False

-- task end

-- MonadTrans instance
instance MonadTrans (ExceptT' e) where
  lift :: (Monad m) => m a -> ExceptT' e m a
  lift m = ExceptT' $ Right <$> m

-- task begin
askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s =
  length s >= 8
    && any isNumber s
    && any isPunctuation s

newtype PwdError = PwdError String deriving (Semigroup, Monoid)

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  if length s < 8
    then printAndThrow "Incorrect input: password is too short!"
    else
      if not (any isNumber s)
        then printAndThrow "Incorrect input: password must contain some digits!"
        else
          if not (any isPunctuation s)
            then printAndThrow "Incorrect input: password must contain some punctuation!"
            else return s
  where
    printAndThrow s = do
      liftIO $ putStrLn s
      throwE $ PwdError s

-- task end

-- task begin
data ReadError = EmptyInput | NoParse String
  deriving (Show)

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
-- tryRead s = case readMaybe s of
--   Nothing ->
--     if null s
--       then throwE $ EmptyInput
--       else throwE $ NoParse $ s
--   Just parsed -> return parsed
tryRead "" = throwE EmptyInput
tryRead s = except (readEither s) `catchE` (\_ -> throwE $ NoParse s)

-- task end

test :: Integer -> ExceptT e (StateT Integer Identity) a -> (Either e a, Integer)
test initState exc = runIdentity $ runStateT (runExceptT exc) initState

-- (Right (), 5)
example2 :: (Either e (), Integer)
example2 = test 3 exc
  where
    exc = except (Right 42) >> lift (put 5)

-- (Right (), 9)
example3 :: (Either e (), Integer)
example3 = test 3 exc
  where
    exc = except (Right 42) >> lift (modify (^ 2))

-- (Left "RORRE", 3)
example4 :: (Either String (), Integer)
example4 = test 3 exc
  where
    exc = throwE "RORRE" >> lift (modify (1 -))

-- (Left "Rorre!", 9)
example5 :: (Either String (), Integer)
example5 = test 3 exc
  where
    exc = lift (modify (^ 2)) >> throwE "Rorre!" >> lift (put (-1))

-- (Left "Rorre!", 9)
example6 :: (Either String (), Integer)
example6 = test 3 exc
  where
    exc = lift (modify (^ 2)) *> throwE "Rorre!" *> lift (modify (1 -))
