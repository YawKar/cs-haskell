
main :: IO ()
main = do
  putStrLn $ toPrettyString (5 :: Integer)
  print int
  print listOfIntegers
  print pair
  print readAllPossibleVariantsOrEmptyList
  print (Main.succ (5 :: Integer)) -- 6
  print (Main.pred (5 :: Integer)) -- 4
  print (Main.toEnum 5 :: Integer) -- 5
  print (Main.fromEnum (5 :: Integer)) -- 5

-- extending Printable type class with PrettyPrintable
class Printable a where
  toString :: a -> String

class Printable a => PrettyPrintable a where
  toPrettyString :: a -> String

-- and instantiating them for Integer type
instance Printable Integer where
  toString = show

instance PrettyPrintable Integer where
  toPrettyString = wrap . toString
    where
      wrap str = "Integer {" ++ str ++ "}"

-- exercise about Gork & Mork
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab val =
        case (doesEnrageMork val, doesEnrageGork val) of
            (True, True) -> stomp . stab $ val
            (True, False) -> stomp val
            (False, True) -> stab val
            _ -> val

-- Read type class is polymorphic. There's a function `read` that's polymorphic by its return type
int = read "5" :: Integer -- it's necessary to provide type via context because of `read :: String -> a`
listOfIntegers = read "[1 ,   2,3, 4]" :: [Integer]
pair = read "(1, True)" :: (Integer, Bool)

-- returns [(5," rings")]
readAllPossibleVariantsOrEmptyList = reads "5 rings" :: [(Integer, String)]

class MyEnum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int

instance MyEnum Integer where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromIntegral

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc cur =
      if maxBound == cur
      then minBound
      else Prelude.succ cur

  spred :: a -> a
  spred cur =
      if minBound == cur
      then maxBound
      else Prelude.pred cur
