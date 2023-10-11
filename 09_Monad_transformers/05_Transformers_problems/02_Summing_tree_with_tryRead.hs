module Demo () where

import Control.Applicative (liftA3)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Monoid (Sum (Sum, getSum))
import Data.Traversable (fmapDefault, foldMapDefault)
import Text.Read (readMaybe)

-- try read impl
data ReadError = EmptyInput | NoParse String
  deriving (Show)

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead "" = throwError EmptyInput
tryRead s = maybe (throwError $ NoParse s) return $ readMaybe s

-- tree impl
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Fork l x r) = liftA3 Fork (traverse f l) (f x) (traverse f r)

-- actual task
treeSum :: Tree String -> Either ReadError Integer
treeSum (Leaf x) = tryRead x
treeSum (Fork l x r) = do
  lx' <- treeSum l
  x' <- tryRead x
  rx' <- treeSum r
  return (lx' + x' + rx')

-- treeSum t = sum <$> traverse tryRead t

-- treeSum :: Tree String -> Either ReadError Integer
-- treeSum t = case fst res of
--   Left err -> Left err
--   Right _ -> Right $ getSum $ snd res
--   where
--     res :: (Either ReadError (), Sum Integer)
--     res = runWriter . runExceptT $ treeSum' t

-- treeSum' :: Tree String -> ExceptT ReadError (Writer (Sum Integer)) ()
-- treeSum' (Leaf x) = do
--   x' <- tryRead x
--   tell $ Sum x'
-- treeSum' (Fork l x r) = do
--   treeSum' l
--   treeSum' (Leaf x)
--   treeSum' r
