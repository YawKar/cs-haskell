module Demo () where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (digitToInt, isDigit, isLower)

newtype Parser a = Parser {apply :: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f
  where
    f [] = []
    f (c : rest) = [(c, rest)]

char :: Char -> Parser Char
char = Parser . charParser
  where
    charParser c [] = []
    charParser c (head : rest)
      | c == head = [(c, rest)]
      | otherwise = []

oneOf :: String -> Parser Char
oneOf = Parser . oneOfParser
  where
    oneOfParser [] _ = []
    oneOfParser _ [] = []
    oneOfParser (headChar : restChars) str@(head : rest)
      | headChar == head = [(head, rest)]
      | otherwise = oneOfParser restChars str

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser fmapped
    where
      fmapped s = [(f a, s') | (a, s') <- apply p s]

-- TASK BEGIN
newtype Prs a = Prs {runPrs :: String -> Maybe (a, String)}

instance Functor Prs where
  fmap :: (a -> b) -> Prs a -> Prs b
  fmap f p = Prs fun
    where
      fun s = (\(a, s') -> (f a, s')) <$> runPrs p s

instance Applicative Prs where
  pure :: a -> Prs a
  pure x = Prs $ \s -> Just (x, s)

  (<*>) :: Prs (a -> b) -> Prs a -> Prs b
  pab <*> pa = Prs f
    where
      f s = case runPrs pab s of
        Nothing -> Nothing
        Just (fab, s') -> (\(a, s'') -> (fab a, s'')) <$> runPrs pa s'

instance Alternative Prs where
  empty :: Prs a
  empty = Prs $ const Nothing

  (<|>) :: Prs a -> Prs a -> Prs a
  p1 <|> p2 = Prs f
    where
      f s = case runPrs p1 s of
        Nothing -> runPrs p2 s
        just -> just

anyChr :: Prs Char
anyChr = Prs f
  where
    f [] = Nothing
    f (c : cs) = Just (c, cs)

many' :: Prs a -> Prs [a]
many' p = (:) <$> p <*> many' p <|> pure []

many1' :: Prs a -> Prs [a]
many1' p = (:) <$> p <*> many' p

satisfy' :: (Char -> Bool) -> Prs Char
satisfy' predicate = Prs f
  where
    f [] = Nothing
    f (c : cs)
      | predicate c = Just (c, cs)
      | otherwise = Nothing

digit' :: Prs Char
digit' = satisfy' isDigit

nat' :: Prs Int
nat' = (read :: String -> Int) <$> many1' digit'

char'' :: Char -> Prs Char
char'' c = satisfy' (== c)

mult' :: Prs Int
mult' = (*) <$> nat' <* char'' '*' <*> nat'

-- TASK END

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> [(x, s)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pab <*> pa = Parser fun
    where
      fun s = [(f a, rest'') | (f, rest') <- apply pab s, (a, rest'') <- apply pa rest']

-- [((1, '?'), "bcd")]
exampleParsePair :: [((Integer, Char), String)]
exampleParsePair = apply parser "1?bcd"
  where
    parser = (,) <$> (fromIntegral . digitToInt <$> oneOf "0123456789") <*> oneOf "!?@"

-- [('b', "c")]
exampleSkipFirstValue :: [(Char, String)]
exampleSkipFirstValue = apply parser "abc"
  where
    parser = anyChar *> anyChar

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser fun
  where
    fun [] = []
    fun (c : cs)
      | predicate c = [(c, cs)]
      | otherwise = []

-- [('1', "23")]
exampleUseSatisfyWithIsDigit :: [(Char, String)]
exampleUseSatisfyWithIsDigit = apply (satisfy isDigit) "123"

lower :: Parser Char
lower = satisfy isLower

-- [('a', "bcde")]
exampleUseLower :: [(Char, String)]
exampleUseLower = apply lower "abcde"

char' :: Char -> Parser Char
char' c = satisfy (== c)

-- [('@', "!?")]
exampleUseChar' :: [(Char, String)]
exampleUseChar' = apply (char' '@') "@!?"

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- [(1, "23")]
exampleUseDigit :: [(Int, String)]
exampleUseDigit = apply digit "123"

-- takes a digit, then skips one '*' char, and then takes another digit
multiplication :: Parser Int
multiplication = (*) <$> digit <* char' '*' <*> digit

-- [(12, "")]
exampleUseMultiplication :: [(Int, String)]
exampleUseMultiplication = apply multiplication "4*3"

-- TASK 2 BEGIN
newtype PrsE a = PrsE {runPrsE :: String -> Either String (a, String)}

instance Functor PrsE where
  fmap :: (a -> b) -> PrsE a -> PrsE b
  fmap f p = PrsE fun
    where
      fun s = case runPrsE p s of
        Left error -> Left error
        Right (a, s') -> Right (f a, s')

instance Applicative PrsE where
  pure :: a -> PrsE a
  pure x = PrsE f
    where
      f s = Right (x, s)

  (<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
  pab <*> pa = PrsE f
    where
      f s = case runPrsE pab s of
        Left err -> Left err
        Right (ab, s') -> (\(a, s) -> (ab a, s)) <$> runPrsE pa s'

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE f
  where
    f [] = Left "unexpected end of input"
    f (c : cs)
      | predicate c = Right (c, cs)
      | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

-- Right ('A', "BC")
testCharE1 :: Either String (Char, String)
testCharE1 = runPrsE (charE 'A') "ABC"

-- Left "unexpected A"
testCharE2 :: Either String (Char, String)
testCharE2 = runPrsE (charE 'B') "ABC"

-- Left "unexpected end of input"
testCharE3 :: Either String (Char, String)
testCharE3 = runPrsE (charE 'B') ""

-- TASK 2 END

{-
-- essentially, a monoid type class for applicatives
-- laws of monoid are inherited with a few new added
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

-- Operation `<*>` is sort of 'multiplication', whilst `<|>` is kind of 'addition'

infixl 3 <|>

-- repeats Monoid [] implementation
instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r -- works like logical `OR`...
  l       <|> _ = l -- ...and makes it possible to perform early return (e.g. short-circuit)

-- Alternative laws (laws inherited from Monoid aren't listed):
-- (1) Right distributivity of <*>:
-- (1) (f <|> g) <*> a === (f <*> a) <|> (g <*> a)
--
-- (2) Right absorption for <*>:
-- (2) empty <*> a === empty
--
-- (3) Left distributivity of fmap (<$>):
-- (3) f <$> (a <|> b) === (f <$> a) <|> (f <$> b)
--
-- (4) Left absorption of fmap (<$>):
-- (4) f <$> empty === empty
-}

-- Just 3 (bc works like logical `OR`)
exampleMaybeAlternativeMonoidicalUsage :: Maybe Integer
exampleMaybeAlternativeMonoidicalUsage = Nothing <|> Just 3 <|> Just 5 <|> Nothing

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const []

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser f
    where
      -- f s = apply p1 s ++ apply p2 s -- this implementation works like 'parse them both and collect'
      f s = case apply p1 s of -- this implementation works like 'parse using p1 if didn't succeed, try p2'
        [] -> apply p2 s
        p1parsed -> p1parsed

-- parses 0 or more lower letters
lowers :: Parser String
--     (((:) <$> lower) <*> lowers) <|> pure ""
lowers = (:) <$> lower <*> lowers <|> pure ""

-- [("abcd", "ABCD")]
exampleUseLowers1 :: [(String, String)]
exampleUseLowers1 = apply lowers "abcdABCD"

-- [("", "ABCD")]
exampleUseLowers2 :: [(String, String)]
exampleUseLowers2 = apply lowers "ABCD"

-- parses `p` while it succeeds
many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []

lowers' :: Parser String
lowers' = many lower

digits :: Parser [Int]
digits = many digit

-- [([1,2,3,4],"abcd")]
exampleUseDigits :: [([Int], String)]
exampleUseDigits = apply digits "1234abcd"
