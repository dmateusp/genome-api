{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Person (Person(..), PersonApi, personServer, personApi) where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Data.Map.Strict            (fromList, Map, toList)
import           Data.Pool                  (withResource)
import           Control.Monad.Except       (MonadIO, liftIO, lift, join)
import           Control.Monad.Logger       (logDebugNS)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Network.Wai.Handler.Warp
import           Database.Bolt    as DB
import           Servant
import           ServerState                (AppT (..), ServerState (..), runDB)
import           Utils                      (ToTemplateParams(..))


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
instance ToTemplateParams Person where
  toTemplateParams (Person name role slack email) = fromList $ [("name", T name), ("role", T role), ("slack", T slack), ("email", T email)]

-- |Converts some BOLT value to Person
toPerson :: Monad m => DB.Value -> m Person
toPerson p = do node  :: Node <- exact p
                let props = nodeProps node
                name  :: Text <- (props `at` "name")  >>= exact
                role  :: Text <- (props `at` "role")  >>= exact
                slack :: Text <- (props `at` "slack") >>= exact
                email :: Text <- (props `at` "email") >>= exact
                return $ Person name role slack email


-- | Defining API and server
type PersonApi =
             "persons" :> QueryParam "name" Text :> QueryParam "role" Text :> QueryParam "slack" Text :> QueryParam "email" Text :> Get '[JSON] [Person]
        :<|> "person"  :> ReqBody '[JSON] Person :> Put '[JSON] Person

personApi :: Proxy PersonApi
personApi = Proxy

personServer :: MonadIO m => ServerT PersonApi (AppT m)
personServer = queryPersons
         :<|>  upsertPerson


-- | DB Functions
queryPersons :: MonadIO m => Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppT m [Person]
queryPersons name role slack email = do
  logDebugNS "web" "Searching persons"

  records <- runDB $ queryP cypher params
  nodes   <- traverse (`at` "p") records
  traverse toPerson nodes where

    cypher :: Text
    cypher = "MATCH (p:Person) WHERE " <>
               "p.name CONTAINS {name}  OR " <>
               "p.role CONTAINS {role}  OR " <>
               "p.slack CONTAINS {slack} OR " <>
               "p.email CONTAINS {email} " <>
             "RETURN p"

    toParam :: (Maybe Text, Text) -> (Text, DB.Value)
    toParam ((Just val), argName) = (val, T argName)
    toParam (Nothing, argName)    = ("" , T argName)

    params :: Map Text DB.Value
    params = fromList $ toParam <$> [(name, "name"), (role, "role"), (slack, "slack"), (email, "email")]


upsertPerson :: MonadIO m => Person -> AppT m Person
upsertPerson p = do
  logDebugNS "web" $ "Upserting: " <> (toStrict $ decodeUtf8 $ encode p)
  result <- fmap head $ runDB $ queryP cypher (toTemplateParams p)
  person <- result `at` "p" >>= toPerson
  return $ person where

    cypher :: Text
    cypher = "MERGE (p:Person { "<>
               "name: {name} , "   <>
               "role: {role} , "   <>
               "slack: {slack}, "  <>
               "email: {email} "   <>
             "}) "               <>
             "RETURN p"