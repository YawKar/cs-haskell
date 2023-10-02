module Demo () where

import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Monad.Identity (Identity, MonadPlus (mplus, mzero), guard)
import Control.Monad.Trans.Except (Except, ExceptT, catchE, runExcept, throwE, withExcept)
import Text.Read (readMaybe)

newtype Except' e a = Except' {runExcept' :: Either e a}
  deriving (Show)

except :: Either e a -> Except' e a -- util
except = Except'

instance Functor (Except' e) where
  fmap :: (a -> b) -> Except' e a -> Except' e b
  fmap f (Except' eith) = case eith of
    Left err -> except $ Left err
    Right x -> except $ Right $ f x

instance Applicative (Except' e) where
  pure :: a -> Except' e a
  pure = except . Right

  (<*>) :: Except' e (a -> b) -> Except' e a -> Except' e b
  Except' efab <*> Except' ea = case efab of
    Left err -> except $ Left err
    Right fab -> case ea of
      Left err' -> except $ Left err'
      Right a -> except $ Right $ fab a

instance Monad (Except' e) where
  (>>=) :: Except' e a -> (a -> Except' e b) -> Except' e b
  Except' eith >>= k = case eith of
    Left err -> except $ Left err
    Right a -> k a

withExcept' :: (e -> e') -> Except' e a -> Except' e' a
withExcept' trE (Except' eith) = case eith of
  Left e -> except $ Left $ trE e
  Right a -> except $ Right a

throwE' :: e -> Except' e a
throwE' = except . Left

catchE' :: Except' e a -> (e -> Except' e' a) -> Except' e' a
Except' eith `catchE'` handler = case eith of
  Left err -> handler err
  Right a -> except $ Right a

-- Except' {runExcept = Left "Error occured: broken calculations"}
example1 :: Except' String Integer
example1 = do { throwE' "broken calculations" } `catchE'` (throwE' . ("Error occured: " ++))

{-
catchE law:
catchE (throwE e) h === h e -- natural error handling
-}

data DivByError
  = ErrZero String
  | ErrOther
  deriving (Eq, Show)

(/?!) :: Double -> Double -> Except' DivByError Double
x /?! 0 = throwE' $ ErrZero (show x ++ "/0;")
x /?! y = return $ x / y

example2 :: Double -> Double -> Except' DivByError String
example2 x y = action `catchE'` handler
  where
    action = do
      q <- x /?! y
      return $ show q
    handler e = return $ show e

data ListIndexError
  = ErrIndexTooLarge Int
  | ErrNegativeIndex
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
xs !!! i
  | i < 0 = throwE ErrNegativeIndex
  | null xs = throwE $ ErrIndexTooLarge i
  | i == 0 = return $ head xs
  | otherwise = tail xs !!! pred i `catchE` handler
  where
    handler :: ListIndexError -> ExceptT ListIndexError Identity a
    handler (ErrIndexTooLarge i') = throwE $ ErrIndexTooLarge i
    handler e = throwE e

data ReadError
  = EmptyInput
  | NoParse String
  deriving (Show)

tryRead :: (Read a) => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s = case readMaybe s of
  Nothing -> throwE $ NoParse s
  Just parsed -> return parsed

data SumError = SumError Int ReadError
  deriving (Show)

trySum :: [String] -> Except SumError Integer
trySum xs = helper xs 1
  where
    helper :: [String] -> Int -> Except SumError Integer
    helper [] _ = return 0
    helper (x : xs) i = do
      x' <- withExcept handler $ tryRead x
      restSum <- helper xs (i + 1)
      return $ x' + restSum
      where
        handler :: ReadError -> SumError
        handler = SumError i

trySum' :: [String] -> Except SumError Integer
trySum' xs = sum <$> traverse (\(i, s) -> withExcept (SumError i) $ tryRead s) (zip [1 ..] xs)

instance (Monoid e) => Alternative (Except' e) where
  empty :: Except' e a
  empty = mzero
  (<|>) :: Except' e a -> Except' e a -> Except' e a
  (<|>) = mplus

instance (Monoid e) => MonadPlus (Except' e) where
  mzero :: Except' e a
  mzero = throwE' mempty
  mplus :: Except' e a -> Except' e a -> Except' e a
  Except' exc1 `mplus` Except' exc2 = except $
    case exc1 of
      Left e -> either (Left . (e <>)) Right exc2
      r -> r

instance Semigroup DivByError where
  (<>) :: DivByError -> DivByError -> DivByError
  ErrZero s1 <> ErrZero s2 = ErrZero $ s1 <> s2
  ErrZero s1 <> ErrOther = ErrZero s1
  ErrOther <> ErrZero s2 = ErrZero s2
  ErrOther <> ErrOther = ErrOther

instance Monoid DivByError where
  mempty :: DivByError
  mempty = ErrOther

(/?) :: Double -> Double -> Except DivByError Double
x /? 0 = throwE $ ErrZero $ show x ++ "/0;"
x /? y = return $ x / y

example3 :: Double -> Double -> Except DivByError String
example3 x y = action `catchE` handler
  where
    action = do
      q <- x /? y
      guard $ y >= 0
      return $ show q
    handler (ErrZero s) = return s
    handler ErrOther = return "NONNEGATIVE GUARD"

-- Left (ErrZero "5.0/0;7.0/0;2.0/0;")
example4 :: Either DivByError Double
example4 = runExcept $ asum [5 /? 0, 7 /? 0, 2 /? 0]

-- Right 4.0
example5 :: Either DivByError Double
example5 = runExcept $ asum [5 /? 0, 7 /? 0, 2 /? 0, 8 /? 2]

newtype SimpleError = Simple {getSimple :: String}
  deriving (Eq, Show)

instance Semigroup SimpleError where
  (<>) :: SimpleError -> SimpleError -> SimpleError
  Simple s1 <> Simple s2 = Simple $ s1 <> s2

instance Monoid SimpleError where
  mempty :: SimpleError
  mempty = Simple ""

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"

newtype Validate e a = Validate {getValidate :: Either [e] a}

validateSum :: [String] -> Validate SumError Integer
validateSum [] = Validate $ Right 0
validateSum ss = asum' $ zipWith (\i s -> collectE $ withExcept (handler i) $ tryRead s) [1 ..] ss
  where
    asum' :: [Validate SumError Integer] -> Validate SumError Integer
    asum' [] = Validate $ Right 0
    asum' (Validate eithX : xs) = Validate $
      case eithRest of
        Left errRest -> case eithX of
          Left errX -> Left $ errX <> errRest
          Right _ -> Left errRest
        Right resRest -> case eithX of
          Left errX -> Left errX
          Right resX -> Right $ resX + resRest
      where
        Validate eithRest = asum' xs
    handler = SumError

collectE :: Except e a -> Validate e a
collectE exc = case runExcept exc of
  Left err -> Validate $ Left [err]
  Right a -> Validate $ Right a
