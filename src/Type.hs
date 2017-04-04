module Type
    ( AppHandler (..)
    , AppEnv (..)
    ) where

import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import qualified Hasql.Pool as PgPool
import Servant

newtype AppEnv = AppEnv {
  db :: PgPool.Pool
}

type AppHandler = ReaderT AppEnv Handler
