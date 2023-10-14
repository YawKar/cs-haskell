{-# OPTIONS_GHC -Wall #-}

module Kata (highAndLow) where

highAndLow :: String -> String
-- call me Mr. Insane
-- highAndLow = unwords . (\(mx, mn) -> map show [mx, mn]) . bimap maximum minimum . twice . map (read :: String -> Integer) . words
highAndLow s = unwords $ map show [maximum ints, minimum ints]
  where
    ints :: [Integer]
    ints = map read $ words s
