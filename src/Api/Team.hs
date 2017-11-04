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

type TeamApi = "teams"
               :> Get '[JSON] [Team]

teamServer :: Server TeamApi
teamServer = return teams

-- | Db functions
queryTeams :: Maybe String -> Maybe String -> Maybe String -> Handler [Team]
queryTeams name role githubChanel = return teams

teams :: [Team]
teams = [Team "x" "y" "z"]