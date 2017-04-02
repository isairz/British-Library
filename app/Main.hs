{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (startApp)

import Data.Monoid ((<>))
import Hasql.Pool (Settings)

-- TODO: Need config
pgSetting :: Settings
pgSetting = (4, 10, "host=" <> "localhost" <> " port=5432 user=isair dbname=knowledge")

main :: IO ()
main = startApp pgSetting 3000
