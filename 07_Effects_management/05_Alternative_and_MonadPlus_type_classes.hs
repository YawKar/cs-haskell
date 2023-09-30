module Demo () where

import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Monad (MonadPlus (mplus), guard, mfilter, msum)

{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mzero = empty -- empty from Alternative! not from Monoid (mempty)
  mplus :: m a -> m a -> m a
  mplus = (<|>) -- (<|>) from Alternative!

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l <|> _ = l -- takes the leftmost one

instance MonadPlus Maybe

-}

-- Just 2
example1 :: Maybe Integer
example1 = Nothing <|> Just 2 <|> Just 3

-- Just 2
example2 :: Maybe Integer
example2 = Nothing `mplus` Just 2 `mplus` Just 3

{-
instance Alternative [] where
  empty = []
  (<|>) = (++)

instance MonadPlus []
-}

-- [1 .. 6]
example3 :: [Integer]
example3 = [1 .. 3] <|> [4 .. 6]

-- [4, 5, 4, 5, 4, 5]
example4 :: [Integer]
example4 = [1 .. 3] *> [4, 5] -- equivalent to [1 .. 3] >> [4, 5]

-- [1, 1, 2, 2, 3, 3]
example5 :: [Integer]
example5 = [1 .. 3] <* [4, 5]

{-
MonadPlus laws:
(1) Left Zero:
    mzero >>= k === mzero
(2) Right Zero:
    v >> mzero === mzero
(3) Left Distribution:
    (a `mplus` b) >>= k === (a >>= k) `mplus` (b >>= k)
(4) Left Catch:
    return a `mplus` b === return a
In fact, among 3rd and 4th laws only one should be satisfied

for example for lists:
  return 2 `mplus` [1, 2, 3] === [2, 1, 2, 3] -- Left Catch law isn't hold
for example for Maybe:
  return 2 `mplus` Just 3 === Just 2 -- Left Catch law is hold
-}

newtype PrsEP a = PrsEP {runPrsEP :: Int -> String -> (Int, Either String (a, String))}

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP f
  where
    f pos str =
      case str of
        [] -> eoi
        (c : cs)
          | pr c -> success c cs
          | otherwise -> unexp c
      where
        nPos = pos + 1
        eoi = (nPos, Left $ "pos " <> show nPos <> ": unexpected end of input")
        unexp c = (nPos, Left $ "pos " <> show nPos <> ": unexpected " <> [c])
        success x xs = (nPos, Right (x, xs))

instance Functor PrsEP where
  fmap :: (a -> b) -> PrsEP a -> PrsEP b
  fmap f (PrsEP p) = PrsEP newP
    where
      newP pos str = case p pos str of
        (pos', res) -> (pos', outcome)
          where
            outcome = case res of
              Left err -> Left err
              Right (a, rest) -> Right (f a, rest)

instance Applicative PrsEP where
  pure :: a -> PrsEP a
  pure = PrsEP . \x pos str -> (pos, Right (x, str))

  (<*>) :: PrsEP (a -> b) -> PrsEP a -> PrsEP b
  PrsEP pfab <*> PrsEP pa = PrsEP newP
    where
      newP pos0 str0 = case pfab pos0 str0 of
        (pos1, Left err) -> (pos1, Left err)
        (pos1, Right (fab, str1)) ->
          case pa pos1 str1 of
            (pos2, Left err) -> (pos2, Left err)
            (pos2, Right (a, str2)) -> (pos2, Right (fab a, str2))

instance Alternative PrsEP where
  empty :: PrsEP a
  empty = PrsEP $ \pos _ -> (pos, Left $ "pos " ++ show pos ++ ": empty alternative")

  (<|>) :: PrsEP a -> PrsEP a -> PrsEP a
  PrsEP pa <|> PrsEP pb = PrsEP newP
    where
      newP pos str = case (paRes, pbRes) of
        (Left _, Right _) -> (pbPos, pbRes)
        (Right _, _) -> (paPos, paRes)
        _
          | paPos < pbPos -> (pbPos, pbRes)
          | otherwise -> (paPos, paRes)
        where
          (paPos, paRes) = pa pos str
          (pbPos, pbRes) = pb pos str

{-
Haskell2010 :: MonadPlus m => Bool -> m ()
guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty
-}

pythags :: [(Integer, Integer, Integer)]
pythags = do
  z <- [1 ..]
  x <- [1 .. z]
  y <- [x .. z]
  guard (x ^ 2 + y ^ 2 == z ^ 2) -- very useful indeed!
  return (x, y, z)

{-
mconcat :: Monoid m => [m] -> m
mconcat = foldr (<>) mempty

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr (<>) mempty

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = asum -- :: (Foldable t, Alternative f) => t (f a) -> f a

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
-}

-- Just 3
example6 :: Maybe Integer
example6 = msum [Nothing, Just 3, Just 5, Nothing]

-- Just 3
example7 :: Maybe Integer
example7 = asum [Nothing, Just 3, Just 5, Nothing] -- in modern Haskell, it's better to use `asum` as it provides more general interface

{-
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p ma = do
  a <- ma
  if p a
    then return a
    else mzero -- or `empty` from `Alternative`, but it's better to use `mzero` to emphasize the use of MonadPlus
-}

-- Just 4
example8 :: Maybe Integer
example8 = mfilter (> 3) $ Just 4

-- Nothing
example9 :: Maybe Integer
example9 = mfilter (> 3) $ Just 2

-- [5 .. 10]
example10 :: [Integer]
example10 = mfilter (> 4) [1 .. 10]
