{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text
import           Database.Persist
import           Servant.API

import           Models

type Api =
       "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> Capture "name" Text :> Get '[JSON] (Maybe User)
  :<|> "user" :> Get '[JSON] [User]
  :<|> "manga" :> sudo
  -- :<|> "post" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  -- :<|> "post" :> Capture "name" Text :> Get '[JSON] (Maybe User)
  -- :<|> "post" :> Get '[JSON] [User]

api :: Proxy Api
api = Proxy
