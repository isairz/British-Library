module Main where

import App (startApp)

-- import Data.Monoid ((<>))
-- import qualified Hasql.Pool as HP

-- TODO: Need config
-- dbSetting :: HP.Settings
-- dbSetting = (4, 10, "host=" <> "localhost" <> " port=5432 user=isair dbname=knowledge")

main :: IO ()
main = startApp 3000
