{-# OPTIONS_GHC -Wall #-}

module Snail where

snail :: [[Int]] -> [Int]
snail [] = []
snail [[]] = []
snail [m] = m
snail mat = scalp ++ snail innermat
  where
    scalp = head mat ++ _lastColumn_ ++ reverse (last mat) ++ _firstColumn_
    innermat = [init (tail xs) | xs <- init (tail mat)]
    _lastColumn_ = [last xs | xs <- init (tail mat)]
    _firstColumn_ = [head xs | xs <- tail (reverse (tail mat))]
