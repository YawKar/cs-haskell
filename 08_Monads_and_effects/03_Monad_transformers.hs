module Demo () where

import Control.Monad (forever, guard, when)
import Control.Monad.State (MonadState (get, put), State, StateT (runStateT), runState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), asks, runReader, withReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (Writer, WriterT (runWriterT), runWriter, tell)
import Data.Char (toUpper)

secondElem :: Reader [String] String
secondElem = asks (map toUpper . head . tail)

-- "THIS"
example1 :: String
example1 = runReader secondElem $ words "Hello, this is the world"

logFirst :: [String] -> Writer String String
logFirst xs = do
  let first = head xs
  let second = (map toUpper . head . tail) xs
  tell first
  tell second
  return second

-- ("IS", "ThisIS") -- first is the value, second is the log
example2 :: (String, String)
example2 = runWriter $ logFirst $ words "This is the world"

logFirstAndReadSecond ::
  ReaderT -- transformer
    [String] -- environment
    (Writer String) -- inner monad
    String -- return type of the composite monad
logFirstAndReadSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ tell el1
  return el2

-- WriterT (Identity ("IS", "This"))
example3 :: Writer String String -- `Writer` is a type synonym for `WriterT String Identity String`
example3 = runReaderT logFirstAndReadSecond (words "This is the environment")

-- ("IS", "This")
example4 :: (String, String)
example4 = runWriter $ runReaderT logFirstAndReadSecond (words "This is the environment")

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  el1 <- lift $ asks head
  tell el1
  el2 <- lift $ asks (map toUpper . head . tail)
  return el2

-- ("IS", "this")
example5 :: (String, String)
example5 = runReader (runWriterT logFirstAndRetSecond) (words "this is the environment")

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 as = do
  lift $ tell $ filter p2 as
  tell $ filter p1 as
  return $ filter (\x -> not (p1 x || p2 x)) as

type MyRW a = ReaderT [String] (Writer String) a -- our transformer

logFirstAndRetSecond' :: MyRW String
logFirstAndRetSecond' = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ tell el1
  return el2

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW r env = runWriter (runReaderT r env)

-- (10, "Read first two words")
example6 :: (Integer, String)
example6 = flip runMyRW ["Hello", "world", "this", "is", "me"] $ do
  firstWord <- asks head
  secondWord <- asks (head . tail)
  lift $ tell "Read first two words"
  return $ fromIntegral $ length $ firstWord ++ secondWord

myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell

-- (10, "Read first two words")
example7 :: (Integer, String)
example7 = flip runMyRW ["Hello", "world", "this", "is", "me"] $ do
  firstWord <- myAsks head
  secondWord <- myAsks (head . tail)
  myTell "Read first two words"
  return $ fromIntegral $ length $ firstWord ++ secondWord

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt = runWriterT . runReaderT rwt

myAsks' :: (Monad m) => ([String] -> a) -> ReaderT [String] (WriterT String m) a
myAsks' = asks

myTell' :: (Monad m) => String -> ReaderT [String] (WriterT String m) ()
myTell' = lift . tell

myLift' :: (Monad m) => m a -> ReaderT [String] (WriterT String m) a
myLift' = lift . lift

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsks' id
  let evens = filter (even . length) xs
  let odds = filter (odd . length) xs
  guard (length (take 2 evens) == 2)
  guard (length (take 2 odds) == 2)
  myTell' $ head evens ++ "," ++ head odds
  return (map toUpper $ evens !! 1, map toUpper $ odds !! 1)

myWithReader :: (Monad m) => ([String] -> [String]) -> MyRWT m a -> MyRWT m a
myWithReader = withReaderT

veryComplexComputation2 :: MyRWT Maybe (String, String)
veryComplexComputation2 = do
  (e1 : e2 : _) <- myAsks' $ take 2 . filter (even . length) -- may fail and cause Nothing
  (o1 : o2 : _) <- myAsks' $ take 2 . filter (odd . length) -- may fail and cause Nothing
  myTell' (e1 ++ "," ++ o1) >> pure (map toUpper e2, map toUpper o2)

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = runState . runExceptT

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lowerB upperB calc = do
  lift calc
  n <- lift get
  when (n <= lowerB) $ throwE "Lower bound"
  when (n >= upperB) $ throwE "Upper bound"

type RiiEsSiT m = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT ::
  ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a ->
  (Integer, Integer) ->
  Integer ->
  m (Either String a, Integer)
runRiiEsSiT riiEsSit env st = flip runStateT st $ runExceptT $ runReaderT riiEsSit env

go' :: (Monad m) => StateT Integer m Integer -> RiiEsSiT m ()
go' calc = do
  lift $ lift calc
  stateVal <- lift $ lift get
  (lowerB, upperB) <- asks id
  when (stateVal >= upperB) $ lift $ throwE "Upper bound"
  when (stateVal <= lowerB) $ lift $ throwE "Lower bound"

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

-- OFFTOPIC
fac :: (Num a, Enum a) => a -> a
fac n = product [1 .. n]

fac' :: (Foldable t, Num a, Num [a], Enum a) => a -> t [a] -> [a]
fac' n = foldr (*) [1 .. n]

fac'' :: (Eq t, Num t) => t -> t
fac'' 0 = 1
fac'' n = n * fac'' (n - 1)

fib :: (Ord t, Num t, Num a, Enum t) => t -> a
fib n
  | n <= 2 = 1
  | otherwise = fibHelper n 3 1 1 2
  where
    fibHelper targetN curN a0 a1 a2
      | targetN == curN = a2
      | otherwise = fibHelper targetN (succ curN) a1 a2 (a1 + a2)

fib''' :: State (Integer, Integer) Integer
fib''' = do
  (a0, a1) <- get
  let a2 = a0 + a1
  put (a1, a2)
  return a2

-- (2, (1, 2))
example8 :: (Integer, (Integer, Integer))
example8 = runState fib''' (1, 1)

-- OFFTOPIC END
