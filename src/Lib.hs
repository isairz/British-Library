{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

startApp :: Port -> IO ()
startApp port = run port app

-- app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type API = UserAPI :<|> StaticAPI

server :: Server API
server = userServer :<|> staticServer

-- Static
type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

staticServer :: Server StaticAPI
staticServer = serveDirectory "./"

-- User
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "user1" :> Get '[JSON] User
          :<|> "user2" :> Get '[JSON] User

userServer :: Server UserAPI
userServer = usersH :<|> user1H :<|> user2H
  where
    usersH = return users
    user1H = return user1
    user2H = return user2

userApi :: Proxy UserAPI
userApi = Proxy

user1 = User 1 "Isaac" "Newton"
user2 = User 2 "Albert" "Einstein"

users :: [User]
users = [ user1
        , user2
        ]
