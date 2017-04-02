{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Type
    ( App (..)
    ) where

import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import qualified Hasql.Pool as PgPool
import Servant

newtype App a
  = App
  { runApp :: ReaderT PgPool.Pool (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader PgPool.Pool,
               MonadError ServantErr, MonadIO)
