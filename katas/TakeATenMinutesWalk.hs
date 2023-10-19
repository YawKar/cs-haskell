{-# OPTIONS_GHC -Wall #-}

module Codewars.Kata.TenMinuteWalk where

isValidWalk :: String -> Bool
isValidWalk [] = error "empty list was given"
isValidWalk steps = helper 0 (0, 0) steps
  where
    helper :: Int -> (Int, Int) -> String -> Bool
    helper 10 (0, 0) [] = True
    helper 10 _ _ = False
    helper _ _ [] = False
    helper n (x, y) (curStep : restSteps) = case curStep of
      'n' -> helper (n + 1) (x + 1, y) restSteps
      's' -> helper (n + 1) (x - 1, y) restSteps
      'w' -> helper (n + 1) (x, y + 1) restSteps
      'e' -> helper (n + 1) (x, y - 1) restSteps
      unknownDir -> error $ "unknown walk direction: " ++ show unknownDir

isValidWalk' :: String -> Bool
isValidWalk' [] = error "empty list was given"
isValidWalk' steps = length (take 11 steps) == 10 && count 'w' == count 'e' && count 's' == count 'n'
  where
    count c = length . filter (== c) $ steps
