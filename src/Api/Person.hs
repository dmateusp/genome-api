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
import Database.Bolt as Db
import Data.Monoid                ((<>))
import Data.Text                  (Text)
import Data.Map.Strict            (fromList, Map)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Pool                  (Pool, createPool)
import Data.Default
import ServerState                (AppT (..), ServerState (..))
import Control.Monad.Except       (MonadIO, liftIO)

data Person = Person
  { name  :: String
  , role  :: String
  , slack :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

type PersonApi = "persons"
             :> QueryParam "name" Text :> QueryParam "role" Text :> QueryParam "slack" Text :> QueryParam "email" Text
             :> Get '[JSON] [Person]

personServer :: Server PersonApi
personServer = queryPersons

toPerson :: Monad m => Record -> m Person
toPerson p = do
               name  <- (p `at` "name")  >>= exact
               role  <- (p `at` "role")  >>= exact
               slack <- (p `at` "slack") >>= exact
               email <- (p `at` "email") >>= exact
               return $ Person name role slack email


defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4j"}

-- | Db functions
queryPersons :: MonadIO m => Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppT m Person
queryPersons name role slack email = do
  pipe    <- connect defaultConfig
  records <- queryP cypher params
  nodes   <- toPerson <$> records
  nodes where

    toParam :: (Maybe Text, Text) -> (Text, Db.Value)
    toParam ((Just val), argName) = (val, T argName)
    toParam (Nothing, argName)    = ("" , T argName)

    params :: Map Text Db.Value
    params = fromList $ toParam <$> [(name, "name"), (role, "role"), (slack, "slack"), (email, "email")]

    cypher :: Text
    cypher = "MATCH (n:Person) WHERE" <>
               "n.name  =~ {name}  AND" <>
               "n.role  =~ {role}  AND" <>
               "n.slack =~ {slack} AND" <>
               "n.email =~ {email}" <>
             "RETURN n"

addPerson :: Person -> Handler Person
addPerson p = return p



persons = [Person "a" "b" "c" "d"] :: [Person]

-- |Reader monad over IO to store connection pool
type WebM = ReaderT ServerState IO
