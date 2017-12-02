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
  , teamName :: Text  -- relationship
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person
instance ToTemplateParams Person where
  toTemplateParams (Person name role slack email teamName) = fromList $
    [("name", T name),
    ("role", T role),
    ("slack", T slack),
    ("email", T email),
    ("teamName", T teamName)]

-- |Converts some BOLT value to Person
toPerson :: Monad m => DB.Value -> m Person
toPerson p = do node  :: Node <- exact p
                let props = nodeProps node
                name     :: Text <- (props `at` "name")  >>= exact
                role     :: Text <- (props `at` "role")  >>= exact
                slack    :: Text <- (props `at` "slack") >>= exact
                email    :: Text <- (props `at` "email") >>= exact
                teamName :: Text <- (props `at` "teamName") >>= exact  -- that's wrong need to fix
                return $ Person name role slack email teamName


-- | Defining API and server
type PersonApi =
            "person" :> Capture "email" Text :> ReqBody '[JSON] Person :> PutNoContent '[JSON] NoContent

personServer :: MonadIO m => ServerT PersonApi (AppT m)
personServer = upsertPerson

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
    cypher = "MATCH (team:Team {name: {teamName}}) " <>
             "MERGE (p: Person {email: {emailUpdate}})-[r:MEMBER_OF]->(team) " <>
             "SET p.name = {name}, " <>
               "p.role = {role}, "   <>
               "p.slack = {slack}, " <>
               "p.email = {emailUpdate}"
