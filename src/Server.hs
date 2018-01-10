{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Server(app, api) where

import Api.Person                (personServer, PersonApi, Person(..), PersonTeams(..))
import Api.Team                  (teamServer, TeamApi, Team(..), TeamMicroServices(..))
import Api.MicroService as M     (microServiceServer, MicroServiceApi, MicroService(..), MicroServiceDependencies(..))
import Control.Monad.Except
import Control.Lens
import Servant
import Servant.Server
import Servant.Swagger
import Data.Swagger as S
import ServerState               (AppT (..), ServerState (..))
import qualified Database.Bolt   as DB
import Control.Category          ((<<<), (>>>))

-- | API for serving @swagger.json@.
type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

swagger :: Swagger
swagger = toSwagger appApi
  & info.title   .~ "Genome API"
  & info.version .~ "0.1"
  & info.(S.description) ?~ "genome is a project for micro-service-oriented companies to visualize their eco-system of services"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

type AppApi = PersonApi
         :<|> TeamApi
         :<|> MicroServiceApi

type Api = SwaggerApi
      :<|> AppApi

appApi :: Proxy AppApi
appApi = Proxy

api :: Proxy Api
api = Proxy

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function. @NT 'Handler'@ is a natural transformation that
-- effectively specialises app base monad to IO
appToServer :: ServerState -> Server Api
appToServer serverState = enter (convertApp serverState >>> NT Handler) server where

   -- | converts @'AppT' m@ monad into @ExceptT ServantErr
   -- m@ monad that Servant's 'enter' function needs in order to run the
   -- application. The ':~>' type is a natural transformation, or, in
   -- non-category theory terms, a function that converts two type
   -- constructors without looking at the values in the types.
   convertApp :: ServerState -> AppT m :~> ExceptT ServantErr m
   convertApp serverState = runReaderTNat serverState <<< NT runApp

server :: MonadIO m => ServerT Api (AppT m)
server = return swagger
    :<|> personServer
    :<|> teamServer
    :<|> microServiceServer

app :: ServerState -> Application
app serverState = serve api (appToServer serverState)