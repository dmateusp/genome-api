{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Person where

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


queryPersons :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Handler [Person]
queryPersons name role slack email = return $ filter (atLeastOneEqual name role slack email) personsDB where
  atLeastOneEqual :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Person -> Bool
  atLeastOneEqual Nothing Nothing Nothing Nothing _ = True
  atLeastOneEqual n r s e (Person n' r' s' e')      = foldl (||) False $ zipWith (==) [n, r, s, e] (map Just [n', r', s', e'])

addPerson :: Person -> Handler Person
addPerson p = return p

personsDB :: [Person]
personsDB =
  [ Person "Dxxx Mxxx Pxxx" "Data Engineer" "@dxxxx" "xxx@gilt.com"
  , Person "Oxxx Cxxxx"     "Data Engineer" "@oxxxx" "xxxx@gilt.com"
  ]
