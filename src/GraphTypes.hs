{-# LANGUAGE DeriveGeneric       #-}
module GraphTypes where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH

data CreatedNode =
  CreatedNode { created :: Int } deriving (Eq, Show, Generic)

instance ToJSON CreatedNode
instance FromJSON CreatedNode