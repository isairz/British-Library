{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module PostgresJson
  ( PgResult
  , select
  )
where

import qualified Data.ByteString.Char8 as B8
import           Data.HashMap.Lazy
import           Data.Int
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T (encodeUtf8)

import qualified Hasql.Decoders        as HD
import qualified Hasql.Encoders        as HE
import qualified Hasql.Query           as HQ

import           Type                  (SearchParams)

type Schema = T.Text
type TableName = T.Text
type SqlQuery = T.Text
type SqlFragment = T.Text
type PgResult = B8.ByteString

unicodeStatement :: T.Text -> HE.Params a -> HD.Result b -> Bool -> HQ.Query a b
unicodeStatement = HQ.statement . T.encodeUtf8

decoder :: HD.Result PgResult
decoder = HD.singleRow (HD.value HD.bytea)

select :: TableName -> SearchParams -> HQ.Query () PgResult
select table params = createReadStatement $ selectQuery table params

-- where :: SearchParams -> B8.ByteString
-- where = ("WHERE " <> intercalate " AND " ( map (pgFmtFilter qi ) conditions )) `emptyOnNull` conditions

createReadStatement :: SqlQuery -> HQ.Query () PgResult
createReadStatement selectQuery = unicodeStatement selectQuery HE.unit decoder False

selectQuery :: TableName -> SearchParams -> SqlQuery
selectQuery table params =
  "SELECT COALESCE( array_to_json(array_agg(row_to_json(" <>
    table <>
  "))), '[]')::character varying from " <>
    table
