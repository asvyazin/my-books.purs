module Common.Monad where


import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)
import Prelude


guardMaybe :: forall m a e. (MonadError e m) => e -> Maybe a -> m a
guardMaybe err =
  maybe (throwError err) pure


guardEither :: forall m a e. (MonadError e m) => Either e a -> m a
guardEither =
  either throwError pure
