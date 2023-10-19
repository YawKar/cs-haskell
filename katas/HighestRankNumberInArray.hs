{-# OPTIONS_GHC -Wall #-}

module Term where

import Data.List
import Data.Ord (comparing)

highestRank :: (Ord c) => [c] -> c
highestRank = snd . maximumBy cmp . map (\g -> (length g, head g)) . group . sort
  where
    cmp (xn, x) (yn, y)
      | xn == yn = compare x y
      | otherwise = compare xn yn

highestRank' :: (Ord c) => [c] -> c
highestRank' = head . maximumBy (comparing length) . group . sort
