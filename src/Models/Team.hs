{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Team where

data Team = Team
  { name         :: String
  , role         :: String
  , githubChanel :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Team
instance FromJSON Team