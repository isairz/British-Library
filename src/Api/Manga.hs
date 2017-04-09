{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Manga where

import Servant
-- import Servant.API.ContentTypes
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Int
import qualified Data.ByteString.Char8 as B8
import qualified Hasql.Query as HQ
import qualified Hasql.Session as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Pool as PgPool

import JsonString (JsonString)
import Type (AppHandler, AppEnv(..))

type MangaAPI = "mangas" :> Get '[JsonString] B8.ByteString
          --  :<|> "manga1" :> Get '[JSON] String

mangaServer :: ServerT MangaAPI AppHandler
mangaServer = mangas
        --  :<|> mangas

selectMangas :: HQ.Query () B8.ByteString
selectMangas =
  HQ.statement sql HE.unit decoder False
  where
    sql = "SELECT COALESCE( array_to_json(array_agg(row_to_json(manga))), '[]') from manga"
    decoder = HD.singleRow (HD.value HD.bytea)

mangas :: AppHandler B8.ByteString
mangas = do
  pool <- db <$> ask
  ver <- liftIO $ PgPool.use pool (HS.query () selectMangas)
  case ver of
    Left err -> throwError err404
    Right t -> return t
