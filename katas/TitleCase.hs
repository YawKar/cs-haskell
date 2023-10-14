{-# OPTIONS_GHC -Wall #-}

module TitleCase (titleCase) where

import Data.Char (toLower, toTitle)

titleCase :: String -> String -> String
titleCase _ "" = ""
titleCase minor title =
  unwords $
    wordToTitleCase (head titleWords)
      : map (wordToTitleCaseCensored minorWords) (tail titleWords)
  where
    titleWords = words title
    minorWords = words minor
    wordToTitleCaseCensored _ [] = []
    wordToTitleCaseCensored censorship word
      | map toLower word `elem` map (map toLower) censorship = map toLower word
      | otherwise = wordToTitleCase word
    wordToTitleCase [] = []
    wordToTitleCase (c : cs) = toTitle c : map toLower cs
