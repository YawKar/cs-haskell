module Demo () where

import Control.Monad.Except
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
import Data.Foldable

limited :: (MonadState s f, MonadError e f, Num e, Enum e) => (s -> Bool) -> [State s b] -> f [b]
limited p fs = traverse limit1 (zip [0 ..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

run1 :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1 f s = runIdentity $ flip runStateT s $ runExceptT f

run2 :: StateT s (Except Int) [a] -> s -> Either Int ([a], s)
run2 f s = runIdentity $ runExceptT $ runStateT f s

run1' :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1' = runState . runExceptT

run2' :: StateT s (Either Int) [a] -> s -> Either Int ([a], s)
run2' = runStateT
