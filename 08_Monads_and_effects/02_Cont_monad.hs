module Demo () where

import Control.Monad (when)
import Control.Monad.Cont (Cont)
import Control.Monad.Trans.Cont (callCC)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Text.Read (readMaybe)

-- examples of ordinary haskell functions:

_square :: Int -> Int
_square = (^ 2)

_add :: Int -> Int -> Int
_add x y = x + y

-- example of continuation passing style functions

square :: Int -> (Int -> r) -> r
square x c = c $ x ^ 2

add' :: Int -> Int -> (Int -> r) -> r
add' x y c = c $ x + y

-- 256
example1 :: Int
example1 = square 4 square id

-- 12 -- 2 ^ 2 + 3 + 5
example2 :: Int
example2 = square 2 (add' 3) (add' 5) id

-- decode :: (Int -> r) -> r
-- decode c = c 0

-- as :: Int -> (Int -> r) -> r
-- as x c = c x

-- a :: Int -> (Int -> r) -> r
-- a x c = c x

-- number :: a -> a
-- number = id

-- one :: Int -> (Int -> r) -> r
-- one x c = c $ x + 1

-- two :: Int -> (Int -> r) -> r
-- two x c = c $ x + 2

-- three :: Int -> (Int -> r) -> r
-- three x c = c $ x + 3

-- seventeen :: Int -> (Int -> r) -> r
-- seventeen x c = c $ x + 17

-- twenty :: Int -> (Int -> r) -> r
-- twenty x c = c $ x + 20

-- hundred :: Int -> (Int -> r) -> r
-- hundred x c = c $ x * 100

-- thousand :: Int -> (Int -> r) -> r
-- thousand x c = c $ x * 1000

decode :: (Int -> c) -> c
decode c = c 0

mul :: Int -> Int -> (Int -> r) -> r
mul x y c = c $ x * y

add :: Int -> Int -> (Int -> r) -> r
add x y c = c $ x + y

as :: Int -> (Int -> r) -> r
as x c = c x

a = as

number = id

one = add 1

two = add 2

three = add 3

seventeen = add 17

twenty = add 20

thousand = mul 1000

hundred = mul 100

example3 :: Int
example3 = decode one hundred twenty three as a number

example4 :: Int
example4 = decode one hundred twenty one as a number

example5 :: Int
example5 = decode one hundred twenty as a number

example6 :: Int
example6 = decode one hundred as a number

example7 :: Int
example7 = decode three hundred as a number

example8 :: Int
example8 = decode two thousand seventeen as a number

sumSquares :: Int -> Int -> (Int -> r) -> r
sumSquares x y c =
  square x $ \x2 ->
    square y $ \y2 ->
      add x2 y2 $ \ss ->
        c ss

newtype Cont' r a = Cont {runCont :: (a -> r) -> r}

evalCont :: Cont' r r -> r
evalCont m = runCont m id

squareCont :: Int -> Cont' r Int
squareCont x = Cont $ \c -> c $ x ^ 2

addCont :: Int -> Int -> Cont' r Int
addCont x y = Cont $ \c -> c $ x + y

showCont :: (Show a) => Cont' String a -> String
showCont m = runCont m show

instance Functor (Cont' r) where
  fmap :: (a -> b) -> Cont' r a -> Cont' r b
  fmap ab (Cont arr) = Cont $ \br -> arr (br . ab)

instance Applicative (Cont' r) where
  pure :: a -> Cont' r a
  pure = Cont . flip ($)

  (<*>) :: Cont' r (a -> b) -> Cont' r a -> Cont' r b
  Cont abrr <*> Cont arr = Cont $ \br -> arr (\a -> abrr (\ab -> br $ ab a))

instance Monad (Cont' r) where
  (>>=) :: Cont' r a -> (a -> Cont' r b) -> Cont' r b
  Cont arr >>= k = Cont $ \br -> arr (\a -> runCont (k a) br)

sumIt :: Cont' r Int
sumIt = do
  a <- return 3
  b <- return 5
  return $ a + b

-- "8"
example9 :: String
example9 = runCont sumIt show

sumIt' :: Cont' String Int
sumIt' = do
  a <- return 3
  b <- Cont $ const "STOP" -- rewritten return; makes it possible to change the `c` continuation explicitly
  return $ a + b

example10 :: String
example10 = runCont sumIt' show -- should be `Int -> String` for terminated function

sumIt'' :: Cont' [r] Int
sumIt'' = do
  a <- return 3
  b <- Cont $ \c -> c 4 ++ c 5 -- allows to, for example, concat results of next calculations
  return $ a + b

type Checkpointed a = (a -> Cont' a a) -> Cont' a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed p checkp = runCont (checkp k) id
  where
    k a = Cont $
      \c ->
        if p (c a)
          then c a
          else a

addTens :: (Num a, Monad m) => a -> (a -> m a) -> m a
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2 {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3 {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4 {- x4 = x1 + 30 -}

example11 :: Int
example11 = runCheckpointed (< 100) $ addTens 1

example12 :: Int
example12 = runCheckpointed (< 30) $ addTens 1

example13 :: Int
example13 = runCheckpointed (< 20) $ addTens 1

example14 :: Int
example14 = runCheckpointed (< 10) $ addTens 1

newtype FailCont r e a = FailCont {runFailCont :: (a -> r) -> (e -> r) -> r}

instance Functor (FailCont r e) where
  fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
  fmap f (FailCont fc) = FailCont $ \ok err -> fc (ok . f) err

instance Applicative (FailCont r e) where
  pure :: a -> FailCont r e a
  pure x = FailCont $ \ok _ -> ok x

  (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
  FailCont abrErR <*> FailCont arErR = FailCont newFC
    where
      newFC ok err = abrErR abr err
        where
          abr ab = arErR (ok . ab) err

instance Monad (FailCont r e) where
  (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
  FailCont arErR >>= k'aBrErR = FailCont newFC
    where
      newFC ok err = arErR ar err
        where
          ar a = brErR ok err
            where
              FailCont brErR = k'aBrErR a

toFailCont :: Except e a -> FailCont r e a
toFailCont exc = FailCont $
  \ok' err' -> case runExcept exc of
    Left err -> err' err
    Right val -> ok' val

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont aReRr) = aReRr ar er
  where
    ar = Right
    er = Left

data ReadError
  = EmptyInput
  | NoParse String
  deriving (Show)

tryRead :: (Read a) => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s = case readMaybe s of
  Nothing -> throwE $ NoParse s
  Just parsed -> return parsed

add'' :: Int -> Int -> FailCont r e Int
add'' x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

test :: Integer -> Cont' Integer Integer
test x = do
  a <- return 3
  Cont $ \c -> if x > 100 then 42 else c () -- `42` here monomorphises polymorphism and bind `Integer` to result type of the continuation
  return $ a + x

test' :: Integer -> Cont r Integer
test' x = callCC $ \k -> do
  a <- return 3
  when (x > 100) (k 42)
  return $ a + x

-- callCC :: ((Integer -> Cont r b) -> Cont r Integer) -> Cont r Integer -- it is not the general type of callCC!

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont newFC
  where
    newFC ar er = runFailCont (f afreb) ar er
      where
        afreb a = FailCont $ \_ _ -> ar a
