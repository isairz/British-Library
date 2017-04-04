{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Api.User where

import Data.Aeson
import Data.Aeson.TH
import Servant

import Type (AppHandler)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "user1" :> Get '[JSON] User
          :<|> "user2" :> Get '[JSON] User

userServer :: ServerT UserAPI AppHandler
userServer = usersH :<|> user1H :<|> user2H
  where
    usersH = return users
    user1H = return user1
    user2H = return user2

user1 :: User
user1 = User 1 "Isaac" "Newton"
user2 :: User
user2 = User 2 "Albert" "Einstein"

users :: [User]
users = [ user1
        , user2
        ]
