{-# LANGUAGE TypeOperators #-}
module Server(app) where

import Api.Person(personServer)
import Api.Team(teamServer)
import Control.Monad.Except
import Servant((:<|>) ((:<|>)), (:~>) (NT),
                Proxy (Proxy), Raw, ServantErr, Server,
                enter, serve)
import Servant.Server
import Config (AppT (..), Config (..))

type AppApi = PersonApi :<|> TeamApi


-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function. @NT 'Handler'@ is a natural transformation that
-- effectively specialises app base monad to IO
appToServer :: ServerState -> Server AppApi
appToServer serverState = enter (convertApp serverState >>> NT Handler) server where

   server :: Server AppApi
   server = personServer :<|> teamServer

   -- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
   -- m@ monad that Servant's 'enter' function needs in order to run the
   -- application. The ':~>' type is a natural transformation, or, in
   -- non-category theory terms, a function that converts two type
   -- constructors without looking at the values in the types.
   convertApp :: ServerState -> AppT m :~> ExceptT ServantErr m
   convertApp serverState = runReaderTNat serverState <<< NT runApp


-- | Finally, this function takes a configuration and runs our 'API'
app :: ServerState -> Application
app serverState = serve (Proxy :: Proxy AppApi) (appToServer serverState)