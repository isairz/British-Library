module Type where

import           Control.Monad.Reader       (ReaderT)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Lazy
import qualified Data.Text                  as T
import qualified Hasql.Pool                 as PgPool
import           Servant

newtype AppEnv = AppEnv {
  db :: PgPool.Pool
}

type AppHandler = ReaderT AppEnv Handler

type SearchParams = HashMap T.Text [T.Text]
