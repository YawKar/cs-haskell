module Test (sumIt, const42) where

sumIt :: Num a => a -> a -> a
sumIt x y = x + y

const42 :: b -> Integer
const42 = const 42
