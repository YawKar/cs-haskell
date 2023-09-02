import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import qualified Data.List as L
{-
From comment section: usage of phantom types to encode some context in the type of a value
-}
newtype TextField fieldType validationContext = TextField String deriving Show

data Validated
data Unvalidated

data Name
data Email

textFieldCons :: String -> TextField a Unvalidated
textFieldCons = TextField

showNameEmailAny :: TextField Name a -> TextField Email a -> String
showNameEmailAny (TextField name) (TextField email) = "name: " ++ name ++ ", email: " ++ email

showNameEmailOnlyValid :: TextField Name Validated -> TextField Email Validated -> String
showNameEmailOnlyValid (TextField name) (TextField email) = "name: " ++ name ++ ", email: " ++ email

sanitize :: TextField a Unvalidated -> TextField a Validated
sanitize (TextField text) = TextField (filter isAlphaNum text)

n :: TextField Name Unvalidated
n = textFieldCons "Andrey"
e :: TextField Email Unvalidated
e = textFieldCons "someemail@somewhere.org"
nValid :: TextField Name Validated
nValid = sanitize n
eValid :: TextField Email Validated
eValid = sanitize e

-- cannotUse = showNameEmailOnlyValid n e -- Couldn't match type 'Unvalidated' with 'Validated'
canUse = showNameEmailOnlyValid nValid eValid
-- cannotUse' = sanitize nValid -- Couldn't match type 'Validated' with 'Unvalidated'
{- END -}

-- type String = [Char] -- they are interchangable (e.g. just a synonym/alias)

type IntegerList = [Integer]

sumSquares :: IntegerList -> Integer
sumSquares = sum . map (^2)

type AssocList k v = [(k, v)]

personAge :: AssocList (TextField Name Validated) Int
personAge = [
    (sanitize (textFieldCons "YawKar"), 20),
    (sanitize (textFieldCons "Raison"), 20)
    ]

lookup' :: Eq k => k -> AssocList k v -> Maybe v
lookup' key [] = Nothing
lookup' key ((key', val') : xs)
    | key == key' = Just val'
    | otherwise = lookup' key xs

type IntMap = Map.Map Int

newtype IntList = IList [Int] deriving Show

example :: IntList
example = IList [1, 2]

{-
Main differences between newtype and data:
1. newtype is guaranteed to have only 1 data constructor (therefore can be optimized).
   The wrapper type exists only in the code. In the actual runtime there won't be any wrapper types.
2. Lazier than data type
-}

data IntList' = IList' [Int] deriving Show

ignore' :: IntList' -> String
ignore' (IList' _) = "Hello"
willExcept = ignore' undefined

ignore :: IntList -> String
ignore (IList _) = "Hello"
won'tExcept = ignore undefined

-- :k Identity
-- ghci> Identity :: * -> *
-- :t Identity
-- ghci> Identity :: a -> Identity a
-- Essentially, `Identity` is a wrapper, `runIdentity` is the unwrapper for `Identity a` type value
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)

{-
class (Semigroup a =>) Monoid a where
    mempty :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

-- Monoid laws:
1. mempty `mappend` x === x (left neutral element)
2. x `mappend` mempty === x (right neutral element)
(BEWARE: no commutativity! e.g. (x `mappend` y) not necessarily the same as (y `mappend` x), easy example is String)
3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) (associativity / Semigroup law)
-}

instance Semigroup IntList where -- needed because of `Semigroup a => Monoid a` constraint
    (IList xs1) <> (IList xs2) = IList (xs1 ++ xs2)

instance Monoid IntList where
    mempty = IList []
    -- il1 `mappend` il2 = il1 <> il2

newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Sum a) where
    (<>) (Sum f) (Sum s) = Sum (f + s)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Semigroup (Product a) where
    (<>) (Product f) (Product s) = Product (f * s)

instance Num a => Monoid (Product a) where
    mempty = Product 1

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    (Xor b1) <> (Xor b2) = if b1 == b2 then Xor False else Xor True

instance Monoid Xor where
    mempty = Xor False

newtype Pair a b = Pair (a, b)
    deriving (Eq, Show, Ord)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    Pair (a1, a2) <> Pair (b1, b2) = Pair (a1 <> b1, a2 <> b2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair (mempty, mempty)

mappendedConcat = Pair ("Abc", "fg") `mappend` Pair ("de", "h") -- Pair ("Abcde","fgh")

{-
instance Semigroup a => Semigroup (Maybe a) where
    m <> Nothing = m
    Nothing <> m = m
    Just x1 <> Just x2 = Just (x1 <> x2)

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
-}

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Semigroup (First a) where
    First Nothing <> m = m
    m <> _ = m

instance Monoid (First a) where
    mempty = First Nothing

firstNonNothing :: Maybe Integer
firstNonNothing = getFirst . mconcat . map First $ [Nothing, Just 2, Just 4] -- Just 2

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' Nothing <> _ = Maybe' Nothing
    _ <> Maybe' Nothing = Maybe' Nothing
    Maybe' (Just m1) <> Maybe' (Just m2) = Maybe' (Just (m1 <> m2))

instance Monoid a => Monoid (Maybe' a) where
    mempty :: Monoid a => Maybe' a
    mempty = Maybe' (Just mempty)

class MapLike (m :: * -> * -> *) where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup key (ListMap []) = Nothing
    lookup key (ListMap ((key', val') : xs))
        | key == key' = Just val'
        | otherwise = lookup key (ListMap xs)
    insert key val lm@(ListMap []) = ListMap [(key, val)]
    insert key val (ListMap (curP@(key', _) : xs))
        | key == key' = ListMap $ (key, val) : xs
        | otherwise = ListMap $ curP : getListMap (insert key val (ListMap xs))
    delete key lm@(ListMap []) = lm
    delete key (ListMap (curP@(key', _) : xs))
        | key == key' = ListMap xs
        | otherwise = ListMap $ curP : getListMap (delete key (ListMap xs))

-- List of endomorphisms
funcsList :: Num a => [a -> a] -- by default [Integer -> Integer]
funcsList = [(*2), (+5), (^2)]

someCalc :: [Integer]
someCalc = zipWith ($) funcsList [1, 2, 3] -- [2, 7, 9]

-- Endomorphism (is a member of Monoid class with empty element = id function)
newtype Endo' a = Endo' { appEndo' :: a -> a }

instance Semigroup (Endo' a) where
    Endo' f <> Endo' s = Endo' (f . s)

instance Monoid (Endo' a) where
    mempty = Endo' id

add5AndSquare :: Endo' Integer
add5AndSquare = Endo' (^2) <> Endo' (+5)

_225 :: Integer
_225 = appEndo' add5AndSquare 10

concatted :: Endo' Integer
concatted = mconcat . map Endo' $ funcsList

_42 :: Integer
_42 = appEndo' concatted 4

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (const Nothing)
    lookup key (ArrowMap getter) = getter key
    insert key val (ArrowMap getter) = ArrowMap $ \x -> if x == key then Just val else getter x
    delete key (ArrowMap getter) = ArrowMap $ \x -> if x == key then Nothing else getter x
    fromList [] = empty
    fromList ((key, val) : xs) = ArrowMap $ \x -> if x == key then Just val else getArrowMap (fromList xs) x

{- Test cases copied from comments -}
data TestCase a = TestCase {name::String, actual::a, expected::a} deriving Show
data TestResult a = Success | Fail (TestCase a) deriving Show

run :: Eq a => [TestCase a] -> TestResult a
run = mconcat . map test where
    test :: Eq a => TestCase a -> TestResult a
    test (TestCase s x y) | x == y    = Success
                        | otherwise = Fail (TestCase s x y)

assert :: Bool -> String -> TestCase Bool
assert bool name' = TestCase {name = name', actual = bool, expected = True}

instance Semigroup (TestResult a) where
    Success <> x = x
    x <> Success = x
    (Fail testCase) <> _ = (Fail testCase)

instance Monoid (TestResult a) where
    mempty = Success

tests :: [TestCase Bool]
tests = [
    assert (
        isNothing $ lookup 42 (empty::ArrowMap Int ())
    ) "Empty map is empty",

    assert (
        let m = (fromList [('a', 1), ('b', 2), ('c', 3)])::ArrowMap Char Int in
        and [
            isJust 1 $ lookup 'a' m,
            isJust 2 $ lookup 'b' m,
            isJust 3 $ lookup 'c' m,
            isNothing $ lookup 'd' m
        ]
    ) "fromList works",

    assert (
        let m = ArrowMap (\x -> if x `elem` "ab" then Just x else Nothing) in
            and [
                isJust 'a' $ lookup 'a' m,
                isJust 'b' $ lookup 'b' m,
                isNothing $ lookup 'c' m
            ]
    ) "simple lookup works",

    assert (
        let m1 = (fromList [('a', 1), ('b', 2), ('c', 3)])::ArrowMap Char Int
            m2 = (delete 'b' m1)
        in
            and [
                isJust 1 $ lookup 'a' m2,
                isNothing $ lookup 'b' m2,
                isJust 3 $ lookup 'c' m2,
                isNothing $ lookup 'd' m2
            ]
    ) "you can't look up an element after deleting it",

    assert(
        let m1 = (fromList [('a', 1), ('b', 2), ('c', 3)])::ArrowMap Char Int
            m2 = (insert 'd' 4 m1)
        in
            and [
                isJust 1 $ lookup 'a' m2,
                isJust 2 $ lookup 'b' m2,
                isJust 3 $ lookup 'c' m2,
                isJust 4 $ lookup 'd' m2
            ]
    ) "after inserting a new pair, you can look it up"
  ]
  where
    isNothing Nothing  = True
    isNothing _        = False
    isJust x  (Just y) = (x == y)
    isJust _  _        = False
