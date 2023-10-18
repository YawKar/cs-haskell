{-# OPTIONS_GHC -Wall #-}

module Codewars.G964.FindEven where

findEvenIndex :: [Int] -> Int
findEvenIndex arr = helper 0 (zip pref (tail suff))
  where
    pref = scanl (+) 0 arr
    suff = scanr (+) 0 arr

    helper _ [] = -1
    helper n ((p, s) : pss)
      | p == s = n
      | otherwise = helper (succ n) pss
