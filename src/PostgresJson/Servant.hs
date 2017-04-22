{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module PostgresJson.Servant
    ( JsonString
    ) where

import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List.NonEmpty         as NE
import           Data.Typeable              (Typeable)
import qualified Network.HTTP.Media         as M
import           Servant

data JsonString deriving Typeable
instance Accept JsonString where
    contentTypes _ =
      "application" M.// "json" M./: ("charset", "utf-8") NE.:|
      [ "application" M.// "json" ]
instance MimeRender JsonString B8.ByteString where
    mimeRender _ = L8.fromStrict
instance MimeUnrender JsonString B8.ByteString where
    mimeUnrender _ = Right . L8.toStrict
