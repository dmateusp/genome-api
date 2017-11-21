{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Team where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics

data Team = Team
  { name         :: String
  , role         :: String
  , githubChanel :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Team
instance FromJSON Team