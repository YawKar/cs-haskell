{-# OPTIONS_GHC -Wall #-}

module SoFarRight where

import Prelude hiding (foldl, reverse)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z l = foldr (fun f) id l z

fun :: (b -> a -> b) -> a -> (b -> b) -> (b -> b)
fun fl a resf b = resf $ fl b a

ini :: a -> a
ini = id
