{-# OPTIONS_GHC -Wall #-}

module Tribonacci where

tribonacci :: (Num a) => (a, a, a) -> Int -> [a]
tribonacci _ 0 = []
tribonacci (a, b, c) n
  | n <= 3 = take n [a, b, c]
  | otherwise = a : tribonacci (b, c, a + b + c) (pred n)
