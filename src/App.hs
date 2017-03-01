{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.Text
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Network.Wai
import           Network.Wai.Handler.Warp    as Warp

import           Servant

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool = userAddH :<|> userGetH :<|> userListGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name = liftIO $ userGet name
    userListGetH = liftIO userListGet
    userAdd :: User -> IO (Maybe (Key User))
    userAdd newUser =
      flip runSqlPersistMPool pool $ do
        exists <- selectFirst [UserName ==. userName newUser] []
        case exists of
          Nothing -> Just <$> insert newUser
          Just _  -> return Nothing
    userGet :: Text -> IO (Maybe User)
    userGet name =
      flip runSqlPersistMPool pool $ do
        mUser <- selectFirst [UserName ==. name] []
        return $ entityVal <$> mUser
    userListGet :: IO [User]
    userListGet =
      flip runSqlPersistMPool pool $ do
        mUsers <- selectList [] []
        return $ entityVal <$> mUsers

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: ConnectionString -> IO Application
mkApp connstr = do
  pool <- runStderrLoggingT $ createPostgresqlPool connstr 4
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: ConnectionString -> IO ()
run connstr = Warp.run 3000 =<< mkApp connstr
