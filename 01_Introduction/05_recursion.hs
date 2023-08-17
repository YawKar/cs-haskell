import Data.Array
import GHC.Num (Natural)

factorial :: Natural -> Natural
factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' :: Natural -> Natural
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

doubleFact :: Natural -> Natural
doubleFact n
  | n < 2 = 1
  | otherwise = n * doubleFact (n - 2)

showsError :: a
showsError = error "This is an error"

showsUndefined :: a
showsUndefined = undefined

fibonacci :: Integer -> Integer
fibonacci n
  | n == -2 = -1
  | n == -1 = 1
  | n == 0 = 0
  | n == 1 = 1
  | n < 0 = if even n then -(fibonacci (-n)) else fibonacci (-n)
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

betterFib :: Integer -> Integer
betterFib n
  | n > 1 = betterFib (n - 1) + betterFib (n - 2)
  | n < 0 = betterFib (n + 2) - betterFib (n + 1)
  | otherwise = n

print10 = map (show . linearFibonacci') (range (-10, 10))

main = print print10

linearFibonacci' :: Integer -> Integer
linearFibonacci' n
  | n > 1 = helper n 1 0 1
  | n < 0 = if even n then (-(helper (-n) 1 0 1)) else helper (-n) 1 0 1
  | otherwise = n

helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper n curIdx first second
  | curIdx == n = second
  | otherwise = helper n (curIdx + 1) second (first + second)

myLolIdentity :: Integer -> Integer
myLolIdentity = product . (\n -> map (\f -> f n) [abs, signum])

anotherIdentityForIntegers :: Integer -> Integer
anotherIdentityForIntegers n = abs n * signum n
