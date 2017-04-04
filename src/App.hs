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

import Type (AppHandler, AppEnv(..))
import Api (server, API)

app :: AppEnv -> Application
app env = serve (Proxy :: Proxy API) (server env)

mkApp :: PgPool.Settings -> IO Application
mkApp pgSetting = do
  pool <- PgPool.acquire pgSetting
  return $ app (AppEnv pool)

startApp :: PgPool.Settings -> Warp.Port -> IO ()
startApp pgSetting port = Warp.run port =<< mkApp pgSetting
