module Demo () where

import Control.Monad.Except (MonadError (throwError))
import Text.Read (readMaybe)

data ReadError = EmptyInput | NoParse String
  deriving (Show)

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead "" = throwError EmptyInput
tryRead s = maybe (throwError $ NoParse s) return $ readMaybe s
