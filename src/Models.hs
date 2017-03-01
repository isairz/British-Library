{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Aeson             as A
import           Data.Text
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.TH

import           Jsonb

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  name Text
  age  Int
  UniqueName name
  deriving Eq Read Show

Manga
  name       Text
  authors    Jsonb
  groups     Jsonb
  type       Text
  language   Text
  series     Jsonb
  characters Jsonb
  tags       Jsonb
  createdAt  UTCTime default = now()
  updatedAt  UTCTime default = now()
  hidden     Bool
  deriving Eq Read Show

Chapter
  mangaId  MangaId
  name     Text
  page     Int
  deriving Show
|]

instance FromJSON User where
  parseJSON = A.withObject "User" $ \v -> User <$> v .: "name" <*> v .: "age"

instance ToJSON User where
  toJSON (User name age) = object ["name" .= name, "age" .= age]

{-
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Manga
  idx        Int
  name       Text
  authors    Jsonb
  groups     Jsonb
  type       Text
  language   Text
  serieses   Jsonb
  characters Jsonb

Page
  src Text
|]
-}
