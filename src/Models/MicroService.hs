{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.MicroService where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Servant

data MicroService = MicroService
  { name           :: String
  , github         :: String
  , lastCommit     :: String
  , lastDeployment :: String
  , description    :: String
  } deriving (Eq, Show, Generic)



instance ToJSON MicroService
instance FromJSON MicroService