{-# LANGUAGE TypeOperators #-}
module Server where

import Api.Person(PersonApi, personServer)
import Api.Team(TeamApi, teamServer)
import Servant
import Network.Wai.Handler.Warp


runApp :: IO ()
runApp = run 1234 app
  where app :: Application
        app = serve api server

        api :: Proxy AppApi
        api = Proxy

type AppApi = PersonApi :<|> TeamApi

server :: Server AppApi
server = personServer :<|> teamServer