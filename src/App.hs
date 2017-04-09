{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module App
    ( startApp
    , app
    ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Hasql.Pool as PgPool
import qualified Hasql.Session as HS
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Api (server, API)
import Type (AppHandler, AppEnv(..))
import Util (selectVersion)

app :: AppEnv -> Application
app env = serve (Proxy :: Proxy API) (server env)

mkApp :: PgPool.Settings -> IO Application
mkApp pgSetting = do
  pool <- PgPool.acquire pgSetting
  ver <- liftIO $ PgPool.use pool (HS.query () selectVersion)
  -- FIXME: Log pg version or panic if any error
  print ver
  return $ app (AppEnv pool)

startApp :: PgPool.Settings -> Warp.Port -> IO ()
startApp pgSetting port = Warp.run port . logStdoutDev =<< mkApp pgSetting
