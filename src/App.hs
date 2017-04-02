{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module App
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api (server, API)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: Port -> IO ()
startApp port = run port app
