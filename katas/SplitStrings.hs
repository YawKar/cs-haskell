{-# OPTIONS_GHC -Wall #-}

module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution [] = []
solution [c] = [[c, '_']]
solution (c : c' : cs) = [c, c'] : solution cs
