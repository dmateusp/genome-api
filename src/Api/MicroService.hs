{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.MicroService where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics

data MicroService = MicroService
  { name           :: String
  , github         :: String
  , lastCommit     :: String
  , lastDeployment :: String
  , description    :: String
  } deriving (Eq, Show, Generic)

instance ToJSON MicroService
instance FromJSON MicroService