-- Ad-hoc polymorphism
main :: IO ()
main = putStrLn "Hola!"

plus :: Num a => a -> a -> a
plus = (+)

greaterThan :: Ord a => a -> a -> Bool
greaterThan = (>)

partiallyAppliedGreaterThan :: (Num a, Ord a) => a -> Bool
partiallyAppliedGreaterThan = greaterThan 3

partiallyAppliedGreaterThanPair :: (Num t1, Num t2, Ord t1, Ord t2) => (t1, t2) -> Bool
partiallyAppliedGreaterThanPair = greaterThan (1, 2)

class MyEq a where
  (==), (/=) :: a -> a -> Bool -- type class members should have their own implementation of at least one of operators
  -- Minimal complete definition is a minimal set of functions that we need to provide implementations of in order
  -- for an instance to work properly (e.g. without infinite recursion?)
  a /= b = not (a Main.== b) -- default implementation for all `a` types
  a == b = not (a Main./= b) -- default implementation for all `a` types

data Raison = Good | True | Beautiful

instance MyEq Raison where
  (==) :: Raison -> Raison -> Bool
  a == b =
    case (a, b) of
      (Good, Good) -> Prelude.True
      (Main.True, Main.True) -> Prelude.True
      (Beautiful, Beautiful) -> Prelude.True
      _ -> Prelude.False
  (/=) :: Raison -> Raison -> Bool
  a /= b = not (a Main.== b) -- default implementation can be redefined

instance MyEq Bool where
  (==) :: Bool -> Bool -> Bool
  Prelude.True == Prelude.True = Prelude.True
  Prelude.False == Prelude.False = Prelude.True
  _ == _ = Prelude.False
  (/=) :: Bool -> Bool -> Bool
  a /= b = not (a Main.== b)

instance (MyEq a, MyEq b) => MyEq (a, b) where
  (fa, fb) == (sa, sb) = fa Main.== sa && fb Main.== sb

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString x = if x then "true" else "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
