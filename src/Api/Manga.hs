{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Manga where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import           Data.Text.IO           as T
import qualified Hasql.Pool             as PgPool
import qualified Hasql.Query            as HQ
import qualified Hasql.Session          as HS
import           Servant

import           PostgresJson.Query
import           PostgresJson.Servant   (JsonString)
import           Type                   (AppEnv (..), AppHandler)

type MangaAPI = "mangas" :> QueryParamForm SearchParams :> Get '[JsonString] PgResult

mangaServer :: ServerT MangaAPI AppHandler
mangaServer = selectMangas

selectMangas :: SearchParams -> AppHandler PgResult
selectMangas params = do
  pool <- db <$> ask
  liftIO $ T.putStrLn $ selectQuery "manga" params
  result <- liftIO $ PgPool.use pool $ select "manga" params
  case result of
    Left err   -> throwError err404
    Right json -> return json
