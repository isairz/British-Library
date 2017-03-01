{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid                 ((<>))
import           Database.Persist.Postgresql

import           App

connstr :: ConnectionString
connstr = "host=" <> "localhost" <> " port=5432 user=isair dbname=knowledge"

main :: IO ()
main = run connstr
