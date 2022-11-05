{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Static where

import Servant
import Servant.Multipart
import Type (AppHandler)

type API = MultipartForm MultipartData :> Post '[PlainText] String

api :: Proxy API
api = Proxy

server :: MultipartData -> Handler String
server multipartData = return str
  where str = "The form was submitted with "
           ++ show nInputs ++ " textual inputs and "
           ++ show nFiles  ++ " files."
        nInputs = length (inputs multipartData)
        nFiles  = length (files multipartData)
