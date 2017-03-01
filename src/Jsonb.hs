{-# LANGUAGE OverloadedStrings #-}

module Jsonb where

import           Data.Aeson                       as A
import qualified Data.Aeson.Parser                as A
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Lazy             as BSL
import           Data.Text
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql

newtype Jsonb =
  Jsonb A.Value
  deriving (Show, Eq, Read)

instance PersistField Jsonb where
  toPersistValue (Jsonb t) = PersistDbSpecific $ BSL.toStrict $ A.encode t
  fromPersistValue (PersistByteString s) =
    either (Left . pack . ("Could not convert Json " ++)) (Right . Jsonb) $
    AP.eitherResult $ AP.parse A.value s
  fromPersistValue a =
    Left $ pack ("JsonB conversion failed for value " ++ show a)

instance PersistFieldSql Jsonb where
  sqlType _ = SqlOther "jsonb"


infixr 6 @>., <@.
(@>.) :: EntityField record Jsonb -> A.Value -> Filter record
(@>.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "@>"
    jval = Left $ Jsonb val

(<@.) :: EntityField record Jsonb -> A.Value -> Filter record
(<@.) field val = Filter field jval specifier
  where
    specifier = BackendSpecificFilter "<@"
    jval = Left $ Jsonb val
