{-# OPTIONS_GHC -Wall #-}

module Isogram where

import Data.Char (toLower)
import Data.List (nub, sort)

isIsogram :: String -> Bool
isIsogram s = sorted == nub sorted
  where
    sorted = map toLower $ sort s
