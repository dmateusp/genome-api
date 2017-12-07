{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Team (Team(..), TeamApi, teamServer, teamApi) where

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

data Team = Team
  { name         :: Text
  , role         :: Text
  , slackChannel :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Team
instance FromJSON Team
instance ToTemplateParams Team where
  toTemplateParams (Team name role slackChannel) = fromList $
    [("name", T name),
    ("role", T role),
    ("slackChannel", T slackChannel)]

newtype TeamMicroServices =
  TeamMicroServices
  { microServices :: [Text] } deriving (Eq, Show, Generic)
instance ToJSON TeamMicroServices
instance FromJSON TeamMicroServices
instance ToTemplateParams TeamMicroServices where
  toTemplateParams (TeamMicroServices microServices) = fromList $
    [("microServices", L (T <$> microServices))]

-- | Defining API and server
type TeamApi =
            "team" :> Capture "name" Text :> ReqBody '[JSON] Team :> PutNoContent '[JSON] NoContent
       :<|> "team" :> Capture "name" Text :> "microservices" :> ReqBody '[JSON] TeamMicroServices :> PutNoContent '[JSON] NoContent

teamServer :: MonadIO m => ServerT TeamApi (AppT m)
teamServer = upsertTeam
        :<|> updateMicroServices

teamApi :: Proxy TeamApi
teamApi = Proxy

-- | Handlers
upsertTeam :: MonadIO m => Text -> Team -> AppT m NoContent
upsertTeam name' t = do
  logDebugNS "web" $ "Upserting: " <> (toStrict $ decodeUtf8 $ encode t)
  runDB $ queryP_ cypher $ insert "nameUpdate" (T name') $ toTemplateParams t
  return NoContent
  where

    cypher :: Text
    cypher = "MERGE (t: Team {name: {nameUpdate}}) " <>
             "SET t.name = {name}, " <>
                 "t.role = {role}, " <>
                 "t.slackChannel = {slackChannel}"

updateMicroServices :: MonadIO m => Text -> TeamMicroServices -> AppT m NoContent
updateMicroServices name microServices = do
  logDebugNS "web" $ "Updating micro-services for team " <> name <> " " <> (toStrict $ decodeUtf8 $ encode microServices)
  runDB $ queryP_ cypher $ fromList [("name", (T name))]
  runDB $ queryP_ cypher' $ insert "name" (T name) $ toTemplateParams microServices
  return NoContent
  where

    cypher :: Text
    cypher = "MATCH (t:Team { name: {name} })-[r:OWNS]->()" <>
             "DELETE r"

    cypher' :: Text
    cypher' = "MATCH (t:Team),(ms:MicroService)" <>
              "WHERE t.name = {name} AND ms.name in {microServices}" <>
              "CREATE (t)-[r:OWNS]->(ms)" <>
              "RETURN r"