{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant
import Models.Person


type Api = "persons"
             :> QueryParam "name" String :> QueryParam "role" String :> QueryParam "slack" String :> QueryParam "email" String
             :> Get '[JSON] [Person]
      :<|> "persons"
             :> ReqBody '[JSON] Person
             :> Post '[JSON] Person


server :: Server Api
server = queryPersons
    :<|> addPerson