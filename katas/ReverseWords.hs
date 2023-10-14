{-# OPTIONS_GHC -Wall #-}

module Reverse where

import Data.Function (on)
import Data.List (groupBy)

reverseWords :: String -> String
reverseWords = concatMap reverse . groupBy ((&&) `on` (/= ' '))
