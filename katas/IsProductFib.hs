{-# OPTIONS_GHC -Wall #-}

module Codewars.Kata.Fib where

-- import Control.Monad.State (State, evalState, get, put)

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib = r 0 1
  where
    r a b target
      | a * b >= target = (a, b, a * b == target)
      | otherwise = r b (a + b) target

-- productFib :: Integer -> (Integer, Integer, Bool)
-- productFib = flip evalState (0, 1) . fiba

-- fiba :: Integer -> State (Integer, Integer) (Integer, Integer, Bool)
-- fiba target = do
--   (a, b) <- get
--   let prod = a * b
--   if prod < target
--     then do
--       put (b, a + b)
--       fiba target
--     else return (a, b, prod == target)
