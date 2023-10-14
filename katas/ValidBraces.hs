{-# OPTIONS_GHC -Wall #-}

module Codewars.Kata.Braces where

import Data.List (elemIndex)

validBraces :: String -> Bool
validBraces = null . foldr f []
  where
    openings = "({["
    closings = ")}]"
    f chr stack
      | chr `elem` closings = chr : stack
      | elemIndex chr openings == elemIndex (head stack) closings = tail stack
      | otherwise = [chr]
