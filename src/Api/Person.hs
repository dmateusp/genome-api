{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Person (PersonApi, personServer) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai.Handler.Warp
import           Servant
import           GHC.Generics
import           Database.Bolt    as DB
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Map.Strict            (fromList, Map)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Pool                  (withResource)
import           ServerState                (AppT (..), ServerState (..), runDB)
import           Control.Monad.Except       (MonadIO, liftIO, lift)
import           Control.Monad.Logger       (logDebugNS)

data PersonApiError =
  BoltValueToPersonError

data Person = Person
  { name  :: Text
  , role  :: Text
  , slack :: Text
  , email :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

-- |Converts some BOLT value to Person
toPerson :: Monad m => DB.Value -> m Person
toPerson (L [T name, T role, T slack, T email]) = return $ Person name role slack email
toPerson _ = fail "Not a Person value"

type PersonApi = "persons"
             :> QueryParam "name" Text :> QueryParam "role" Text :> QueryParam "slack" Text :> QueryParam "email" Text
             :> Get '[JSON] [Person]

personServer :: MonadIO m => ServerT PersonApi (AppT m)
personServer = queryPersons

-- | Db functions
queryPersons :: MonadIO m => Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppT m [Person]
queryPersons name role slack email = do
  logDebugNS "web" "queryPersons"

  records <- runDB $ queryP cypher params
  nodes   <- traverse (`at` "p") records
  traverse toPerson nodes where

    toParam :: (Maybe Text, Text) -> (Text, DB.Value)
    toParam ((Just val), argName) = (val, T argName)
    toParam (Nothing, argName)    = ("" , T argName)

    params :: Map Text DB.Value
    params = fromList $ toParam <$> [(name, "name"), (role, "role"), (slack, "slack"), (email, "email")]

    cypher :: Text
    cypher = "MATCH (p:Person) WHERE" <>
               "p.name  =~ {name}  AND" <>
               "p.role  =~ {role}  AND" <>
               "p.slack =~ {slack} AND" <>
               "p.email =~ {email}" <>
             "RETURN p"
