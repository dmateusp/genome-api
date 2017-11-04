{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Person (PersonApi, personServer) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics

data Person = Person
  { name  :: String
  , role  :: String
  , slack :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

type PersonApi = "persons"
             :> QueryParam "name" String :> QueryParam "role" String :> QueryParam "slack" String :> QueryParam "email" String
             :> Get '[JSON] [Person]

personServer :: Server PersonApi
personServer = queryPersons

-- | Db functions
queryPersons :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Handler [Person]
queryPersons name role slack email = return $ filter (atLeastOneEqual name role slack email) persons where
  atLeastOneEqual :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Person -> Bool
  atLeastOneEqual Nothing Nothing Nothing Nothing _ = True
  atLeastOneEqual n r s e (Person n' r' s' e')      = foldl (||) False $ zipWith (==) [n, r, s, e] (map Just [n', r', s', e'])

addPerson :: Person -> Handler Person
addPerson p = return p



persons = [Person "a" "b" "c" "d"] :: [Person]