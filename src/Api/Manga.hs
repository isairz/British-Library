{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Manga where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import qualified Hasql.Pool             as PgPool
import qualified Hasql.Query            as HQ
import qualified Hasql.Session          as HS
import           Servant

import           JsonString             (JsonString)
import           PostgresJson           (PgResult, select)
import           Type                   (AppEnv (..), AppHandler, SearchParams)

type MangaAPI = "mangas" :> QueryParamForm SearchParams :> Get '[JsonString] PgResult

mangaServer :: ServerT MangaAPI AppHandler
mangaServer = selectMangas

selectMangas :: SearchParams -> AppHandler PgResult
selectMangas params = do
  pool <- db <$> ask
  result <- liftIO $ PgPool.use pool (HS.query () (select "manga" params))
  liftIO $ print params
  liftIO $ print result
  case result of
    Left err   -> throwError err404
    Right json -> return json
