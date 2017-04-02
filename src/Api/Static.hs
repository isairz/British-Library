{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Static where

import Servant
import Type (App)

type StaticAPI = "static" :> Raw

staticServer :: ServerT StaticAPI App
staticServer = serveDirectory "./"
