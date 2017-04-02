{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module App
    ( startApp
    , app
    ) where

import qualified Hasql.Pool as PgPool
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Api (server, API)

api :: Proxy API
api = Proxy

app :: PgPool.Pool -> Application
app pool = serve api server

mkApp :: PgPool.Settings -> IO Application
mkApp pgSetting = do
  pool <- PgPool.acquire pgSetting
  return $ app pool

startApp :: PgPool.Settings -> Warp.Port -> IO ()
startApp pgSetting port = Warp.run port =<< mkApp pgSetting
