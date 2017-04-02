{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Servant

import Api.User
import Api.Static

server :: Server API
server = userServer :<|> staticServer

type API = UserAPI :<|> StaticAPI
