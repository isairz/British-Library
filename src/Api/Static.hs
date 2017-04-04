{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Static where

import Servant

type StaticAPI = "static" :> Raw

staticServer :: Server StaticAPI
staticServer = serveDirectoryFileServer "./"
