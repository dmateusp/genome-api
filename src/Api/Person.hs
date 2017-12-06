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
import           Data.Map.Strict            (fromList, Map, toList, insert)
import           Data.Pool                  (withResource)
import           Control.Monad.Except       (MonadIO, liftIO, lift, join)
import           Control.Monad.Logger       (logDebugNS)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Network.Wai.Handler.Warp
import           Database.Bolt    as DB
import           Servant
import           ServerState                (AppT (..), ServerState (..), runDB)
import           Utils                      (ToTemplateParams(..))

data Person =
  Person
  { name  :: Text
  , role  :: Text
  , slack :: Text
  , email :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person
instance ToTemplateParams Person where
  toTemplateParams (Person name role slack email) = fromList $
    [("name", T name),
    ("role", T role),
    ("slack", T slack),
    ("email", T email)]

newtype PersonTeams =
  PersonTeams
  { teams :: [Text] } deriving (Eq, Show, Generic)
instance ToJSON PersonTeams
instance FromJSON PersonTeams
instance ToTemplateParams PersonTeams where
  toTemplateParams (PersonTeams teams) = fromList $
    [("teams", L (T <$> teams))]



-- |Converts some BOLT value to Person
toPerson :: Monad m => DB.Value -> m Person
toPerson p = do node  :: Node <- exact p
                let props = nodeProps node
                name     :: Text <- (props `at` "name")  >>= exact
                role     :: Text <- (props `at` "role")  >>= exact
                slack    :: Text <- (props `at` "slack") >>= exact
                email    :: Text <- (props `at` "email") >>= exact
                return $ Person name role slack email


-- | Defining API and server
type PersonApi =
            "person" :> Capture "email" Text :> ReqBody '[JSON] Person :> PutNoContent '[JSON] NoContent
       :<|> "person" :> Capture "email" Text :> "teams" :> ReqBody '[JSON] PersonTeams :> PutNoContent '[JSON] NoContent

personServer :: MonadIO m => ServerT PersonApi (AppT m)
personServer = upsertPerson
          :<|> updateTeams

personApi :: Proxy PersonApi
personApi = Proxy

-- | Handlers
upsertPerson :: MonadIO m => Text -> Person -> AppT m NoContent
upsertPerson email' p = do
  logDebugNS "web" $ "Upserting: " <> (toStrict $ decodeUtf8 $ encode p)
  runDB $ queryP_ cypher $ insert "emailUpdate" (T email') $ toTemplateParams p
  return NoContent
  where

    cypher :: Text
    cypher = "MERGE (p: Person {email: {emailUpdate}}) " <>
             "SET p.name = {name}, " <>
               "p.role = {role}, "   <>
               "p.slack = {slack}, " <>
               "p.email = {emailUpdate}"

updateTeams :: MonadIO m => Text -> PersonTeams -> AppT m NoContent
updateTeams email teams = do
  logDebugNS "web" $ "Updating teams for " <> email <> " " <> (toStrict $ decodeUtf8 $ encode teams)
  runDB $ queryP_ cypher $ fromList [("email", (T email))]
  runDB $ queryP_ cypher' $ insert "email" (T email) $ toTemplateParams teams
  return NoContent
  where

    cypher :: Text
    cypher = "MATCH (p:Person { email: {email} })-[r:MEMBER_OF]->()" <>
             "DELETE r"

    cypher' :: Text
    cypher' = "MATCH (p:Person),(t:Team)" <>
              "WHERE p.email = {email} AND t.name in {teams}" <>
              "CREATE (p)-[r:MEMBER_OF]->(t)" <>
              "RETURN r"

