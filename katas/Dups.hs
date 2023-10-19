{-# OPTIONS_GHC -Wall #-}

module Dups where

import Data.Char (toLower)
import Data.List (group, sort)

duplicateEncode :: String -> String
duplicateEncode s = map f s
  where
    f c
      | toLower c `elem` frequies = ')'
      | otherwise = '('
    frequies = map head . filter ((> 1) . length) . group . sort . map toLower $ s
