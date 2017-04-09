{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Int
import qualified Data.ByteString.Char8 as B8
import qualified Hasql.Query as HQ
import qualified Hasql.Session as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

selectVersion :: HQ.Query () Int32
selectVersion =
  HQ.statement "SELECT current_setting('server_version_num')::integer"
    HE.unit (HD.singleRow $ HD.value HD.int4) False
