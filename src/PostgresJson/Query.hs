{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgresJson.Query
  ( select
  , SearchParams
  , PgResult
  )
where

import qualified Data.ByteString.Char8 as B8
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T (encodeUtf8)

import qualified Hasql.Decoders        as HD
import qualified Hasql.Encoders        as HE
import qualified Hasql.Query           as HQ
import qualified Hasql.Session         as HS

import           Web.FormUrlEncoded    (FromForm, fromForm, unForm)

import           PostgresJson.Type

unicodeStatement :: T.Text -> HE.Params a -> HD.Result b -> Bool -> HQ.Query a b
unicodeStatement = HQ.statement . T.encodeUtf8

decoder :: HD.Result PgResult
decoder = HD.singleRow (HD.value HD.bytea)

select :: TableName -> SearchParams -> HS.Session PgResult
select table params = HS.query () $ createReadStatement $ selectQuery table params

createReadStatement :: SqlQuery -> HQ.Query () PgResult
createReadStatement selectQuery =
  unicodeStatement selectQuery HE.unit decoder False

selectQuery :: TableName -> SearchParams -> SqlQuery
selectQuery table params =
  "SELECT COALESCE( array_to_json(array_agg(row_to_json(" <>
    table <>
  "))), '[]')::character varying from " <>
    table <>
    whereF params


whereF :: SearchParams -> SqlFragment
whereF (SearchParams conditions) = (" WHERE " <> T.intercalate " AND " ( map conditionF conditions )) `emptyOnNull` conditions

conditionF :: Condition -> SqlFragment
conditionF (Condition field operator operand) = field <> opToSqlFragment operator <> operand

emptyOnNull :: T.Text -> [a] -> T.Text
emptyOnNull val x = if null x then "" else val
