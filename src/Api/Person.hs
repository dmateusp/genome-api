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
import           GraphTypes


data PersonApiError =
  BoltValueToPersonError

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
            "person" :> ReqBody '[JSON] Person :> Post '[JSON] CreatedNode

personServer :: MonadIO m => ServerT PersonApi (AppT m)
personServer = addPerson

personApi :: Proxy PersonApi
personApi = Proxy


-- | Handlers
updatePerson :: MonadIO m => Person -> Text -> AppT m Person
updatePerson p email = do
  logDebugNS "web" $ "Updating person with email: " <> email
  result <- fmap head $ runDB $ queryP cypher (toTemplateParams p)
  person <- result `at` "p" >>= toPerson
  return $ person where

    cypher :: Text
    cypher = "CREATE (p:Person { "<>
               "name: {name} , "   <>
               "role: {role} , "   <>
               "slack: {slack}, "  <>
               "email: {email} "   <>
            "}) "                <>
            "RETURN id(p)"

addPerson :: MonadIO m => Person -> AppT m CreatedNode
addPerson p = do
  logDebugNS "web" $ "Adding: " <> (toStrict $ decodeUtf8 $ encode p)
  result <- fmap head $ runDB $ queryP cypher (toTemplateParams p)

  personId <- result `at` "id" >>= exact
  return $ CreatedNode personId where

    cypher :: Text
    cypher = "CREATE (p:Person { "<>
               "name: {name} , "   <>
               "role: {role} , "   <>
               "slack: {slack}, "  <>
               "email: {email} "   <>
             "}) "                <>
             "RETURN ID(p) as id"