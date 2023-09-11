module Main (main) where
import Data.Monoid (Sum (getSum, Sum))

main :: IO ()
main = putStrLn "Writer Monad"

newtype Writer' w a = Writer' { runWriter' :: (a, w) }

-- take pair and return Writer monad
writer' :: (a, w) -> Writer' w a
writer' = Writer'

-- sometimes we are only interested in logs and don't need the actual value
execWriter' :: Writer' w a -> w
execWriter' m = snd $ runWriter' m

evalWriter' :: Writer' w a -> a
evalWriter' (Writer' (a, _)) = a

{-
Notice:
`runWriter` returns both value and logs
`execWriter` returns only logs
-}

instance Monoid w => Functor (Writer' w) where
    fmap :: (a -> b) -> Writer' w a -> Writer' w b
    fmap f (Writer' (a, w)) = Writer' (f a, w)

instance Monoid w => Applicative (Writer' w) where
    pure :: a -> Writer' w a
    pure = Writer' . (, mempty)
    (<*>) :: Writer' w (a -> b) -> Writer' w a -> Writer' w b
    Writer' (transform, w1) <*> Writer' (a, w2) = Writer' (transform a, w1 <> w2)

instance Monoid w => Monad (Writer' w) where
    (>>=) :: Monoid w => Writer' w a -> (a -> Writer' w b) -> Writer' w b
    Writer' (a, w) >>= k = Writer' (a', w <> w')
        where
            Writer' (a', w') = k a

example :: Writer' [String] Integer
example = do
    variable <- writer' (3, ["init 3"])
    variable <- writer' (variable + 4, ["add 4"])
    writer' (variable * 3, ["mult 3"])

_21 :: Integer
_21 = evalWriter' example

_logs :: [String]
_logs = execWriter' example

tell' :: Monoid w => w -> Writer' w ()
tell' w = writer' ((), w)

calc :: (Int -> Int -> Int) -> Int -> Int -> Writer' String Int
calc op arg1 arg2 = do
    let res = arg1 `op` arg2
    tell' "okay;"
    if abs res < 128 then
        return res
    else do
        tell' "overflowed;"
        return res -- tak delat' ne nado, eto dlya primera XD

overflowed :: (Int, String)
overflowed = runWriter' $ calc (+) 33 95

notOverflowed :: (Int, String)
notOverflowed = runWriter' $ calc (+) 33 94

type Shopping = Writer' (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase item cost = tell' $ Sum cost

total :: Shopping -> Integer
total (Writer' (_, sum)) = getSum sum

type Shopping' = Writer' [(Integer, String)] ()

shopping2 :: Shopping'
shopping2 = do
  purchase' "Jeans"   19200
  purchase' "Water"     180
  purchase' "Lettuce"   328

purchase' :: String -> Integer -> Shopping'
purchase' item cost = tell' [(cost, item)]

total' :: Shopping' -> Integer
total' (Writer' (_, purchases)) = sum $ map fst purchases

items' :: Shopping' -> [String]
items' (Writer' (_, purchases)) = map snd purchases
