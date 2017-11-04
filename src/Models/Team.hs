{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Team where

import Servant
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

data Team = Team
  { name         :: String
  , role         :: String
  , githubChanel :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Team
instance FromJSON Team