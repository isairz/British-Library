module Type
    ( AppHandler (..)
    , AppEnv (..)
    ) where

import Servant
import Control.Monad.Reader (ReaderT)
import qualified Hasql.Pool as PgPool

newtype AppEnv = AppEnv {
  db :: PgPool.Pool
}

type AppHandler = ReaderT AppEnv Handler
