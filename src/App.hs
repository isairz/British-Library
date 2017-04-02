{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module App
    ( startApp
    , app
    ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader (runReaderT)
import qualified Hasql.Pool as PgPool
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Type (App, runApp)
import Api (server, API)

app :: PgPool.Pool -> Application
app pool = serve (Proxy :: Proxy API) (appToServer pool)

appToServer :: PgPool.Pool -> Server API
appToServer pool = enter (convertApp pool) server

convertApp :: PgPool.Pool -> App :~> ExceptT ServantErr IO
convertApp pool = Nat (flip runReaderT pool . runApp)

mkApp :: PgPool.Settings -> IO Application
mkApp pgSetting = do
  pool <- PgPool.acquire pgSetting
  return $ app pool

startApp :: PgPool.Settings -> Warp.Port -> IO ()
startApp pgSetting port = Warp.run port =<< mkApp pgSetting
