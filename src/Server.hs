{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server(app, api) where

import Api.Person(personServer, PersonApi)
import Control.Monad.Except
import Servant((:<|>), (:~>) (NT),
                Proxy (Proxy), Raw, ServantErr, Server,
                enter, serve)
import Servant.Server
import ServerState (AppT (..), ServerState (..))
import qualified Database.Bolt                        as DB
import Control.Category ((<<<), (>>>))


type AppApi = PersonApi
api :: Proxy AppApi
api = Proxy

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function. @NT 'Handler'@ is a natural transformation that
-- effectively specialises app base monad to IO
appToServer :: ServerState -> Server AppApi
appToServer serverState = enter (convertApp serverState >>> NT Handler) server where

   -- | converts @'AppT' m@ monad into @ExceptT ServantErr
   -- m@ monad that Servant's 'enter' function needs in order to run the
   -- application. The ':~>' type is a natural transformation, or, in
   -- non-category theory terms, a function that converts two type
   -- constructors without looking at the values in the types.
   convertApp :: ServerState -> AppT m :~> ExceptT ServantErr m
   convertApp serverState = runReaderTNat serverState <<< NT runApp

server :: MonadIO m => ServerT AppApi (AppT m)
server = personServer

app :: ServerState -> Application
app serverState = serve api (appToServer serverState)