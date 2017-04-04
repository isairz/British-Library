{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Servant
import qualified Hasql.Pool as PgPool

import Type (AppHandler, AppEnv)
import Api.User
import Api.Static

server :: AppEnv -> Server API
server env = enter (runReaderTNat env) userServer
         :<|> staticServer

type API = UserAPI
      :<|> StaticAPI
