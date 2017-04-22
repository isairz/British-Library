{-# LANGUAGE OverloadedStrings #-}
module PostgresJson.Type where

import qualified Data.ByteString.Char8 as B8
import           Data.HashMap.Strict   (toList)
import qualified Data.Text             as T

import           Web.FormUrlEncoded    (FromForm, fromForm, unForm)

type Schema = T.Text
type TableName = T.Text
type SqlQuery = T.Text
type SqlFragment = T.Text
type PgResult = B8.ByteString

data Operator = Equals | Gte | Gt | Lte | Lt | Neq | Like | ILike | Is | IsNot |
                TSearch | Contains | Contained | In | NotIn deriving (Eq, Enum)

instance Show Operator where
  show op =  case op of
    Equals    -> "eq"
    Gte       -> "gte"
    Gt        -> "gt"
    Lte       -> "lte"
    Lt        -> "lt"
    Neq       -> "neq"
    Like      -> "like"
    ILike     -> "ilike"
    In        -> "in"
    NotIn     -> "notin"
    IsNot     -> "isnot"
    Is        -> "is"
    TSearch   -> "@@"
    Contains  -> "@>"
    Contained -> "<@"

instance Read Operator where
  readsPrec _ op =  case op of
    "eq"    -> [(Equals, "")]
    "gte"   -> [(Gte, "")]
    "gt"    -> [(Gt, "")]
    "lte"   -> [(Lte, "")]
    "lt"    -> [(Lt, "")]
    "neq"   -> [(Neq, "")]
    "like"  -> [(Like, "")]
    "ilike" -> [(ILike, "")]
    "in"    -> [(In, "")]
    "notin" -> [(NotIn, "")]
    "isnot" -> [(IsNot, "")]
    "is"    -> [(Is, "")]
    "@@"    -> [(TSearch, "")]
    "@>"    -> [(Contains, "")]
    "<@"    -> [(Contained, "")]
    _       -> []

opToSqlFragment :: Operator -> SqlFragment
opToSqlFragment op = case op of
  Equals    -> "="
  Gte       -> ">="
  Gt        -> ">"
  Lte       -> "<="
  Lt        -> "<"
  Neq       -> "<>"
  Like      -> "LIKE"
  ILike     -> "ILIKE"
  In        -> "IN"
  NotIn     -> "NOT IN"
  IsNot     -> "IS NOT"
  Is        -> "IS"
  TSearch   -> "@@"
  Contains  -> "@>"
  Contained -> "<@"

type FieldName = T.Text
type Operand = T.Text
type Operation = (Operator, Operand)
data Condition = Condition FieldName Operator Operand deriving (Show, Eq)

data SearchParams = SearchParams [Condition] deriving (Show, Eq)

instance FromForm SearchParams where
  fromForm f = pure $ SearchParams conditions
    where conditions = concatMap mkCondition (toList $ unForm f)
          mkCondition (k, vs) = map (parseParam k) vs
          parseParam k v = let s = T.break (=='.') v in
            Condition k (read $ T.unpack (fst s) :: Operator) (T.drop 1 $ snd s)
