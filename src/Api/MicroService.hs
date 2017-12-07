{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.MicroService (MicroService(..), MicroServiceApi, microServiceServer, microServiceApi) where

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

data MicroService = MicroService
  { name           :: Text
  , github         :: Text
  , lastCommit     :: Text
  , lastDeployment :: Text
  , description    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON MicroService
instance FromJSON MicroService
instance ToTemplateParams MicroService where
  toTemplateParams (MicroService name github lastCommit lastDeployment description) = fromList $
    [("name", T name),
    ("github", T github),
    ("lastCommit", T lastCommit),
    ("lastDeployment", T lastDeployment),
    ("description", T description)]

newtype MicroServiceDependencies =
  MicroServiceDependencies
  { dependencies :: [Text] } deriving (Eq, Show, Generic)

instance ToJSON MicroServiceDependencies
instance FromJSON MicroServiceDependencies
instance ToTemplateParams MicroServiceDependencies where
  toTemplateParams (MicroServiceDependencies dependencies) = fromList $
    [("dependencies", L (T <$> dependencies))]

-- | Defining API and server
type MicroServiceApi =
            "microservice" :> Capture "name" Text :> ReqBody '[JSON] MicroService :> PutNoContent '[JSON] NoContent
       :<|> "microservice" :> Capture "name" Text :> "dependencies" :> ReqBody '[JSON] MicroServiceDependencies :> PutNoContent '[JSON] NoContent

microServiceServer :: MonadIO m => ServerT MicroServiceApi (AppT m)
microServiceServer = upsertMicroService
                :<|> updateDependencies

microServiceApi :: Proxy MicroServiceApi
microServiceApi = Proxy

-- | Handlers
upsertMicroService :: MonadIO m => Text -> MicroService -> AppT m NoContent
upsertMicroService name' ms = do
  logDebugNS "web" $ "Upserting: " <> (toStrict $ decodeUtf8 $ encode ms)
  runDB $ queryP_ cypher $ insert "nameUpdate" (T name') $ toTemplateParams ms
  return NoContent
  where

    cypher :: Text
    cypher = "MERGE (ms: MicroService {name: {nameUpdate}}) " <>
             "SET ms.name = {name}, " <>
                 "ms.github = {github}, " <>
                 "ms.lastCommit = {lastCommit}, " <>
                 "ms.lastDeployment = {lastDeployment}, " <>
                 "ms.description = {description}"

updateDependencies :: MonadIO m => Text -> MicroServiceDependencies -> AppT m NoContent
updateDependencies name dependencies = do
  logDebugNS "web" $ "Updating dependencies for service " <> name <> " " <> (toStrict $ decodeUtf8 $ encode dependencies)
  runDB $ queryP_ cypher $ fromList [("name", (T name))]
  runDB $ queryP_ cypher' $ insert "name" (T name) $ toTemplateParams dependencies
  return NoContent
  where

    cypher :: Text
    cypher = "MATCH (ms:MicroService { name: {name} })-[r:USES]->()" <>
             "DELETE r"

    cypher' :: Text
    cypher' = "MATCH (a:MicroService),(b:MicroService)" <>
              "WHERE a.name = {name} AND b.name in {dependencies}" <>
              "CREATE (a)-[r:USES]->(b)" <>
              "RETURN r"