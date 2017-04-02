{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Servant

import Type (App)
import Api.User
import Api.Static

server :: ServerT API App
server = userServer
-- :<|> staticServer

type API = UserAPI
-- :<|> StaticAPI
