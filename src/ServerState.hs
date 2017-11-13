{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module ServerState where

import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (MonadLogger (..),
                                                       toLogStr)
import           Control.Monad.Metrics                (Metrics, MonadMetrics,
                                                       getMetrics)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, ask, asks)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Data.Pool                            (Pool, createPool, withResource)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)
import qualified Database.Bolt                        as DB
import           Data.Default                         (def)
import           Logger

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT ServerState', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a
    = AppT
    { runApp :: ReaderT ServerState (ExceptT ServantErr m) a
    } deriving ( Functor, Applicative, Monad, MonadReader ServerState,
                 MonadError ServantErr, MonadIO)

type App = AppT IO

type ConnectionPool = Pool DB.Pipe
-- | The ServerState for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data ServerState
    = ServerState
    { environment    :: Environment
    , metrics        :: Metrics
    , logEnv         :: LogEnv
    , connectionPool :: ConnectionPool
    }

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks ServerState.metrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks ServerState.logEnv
    localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
    -- todo: log proper request data
    logMsg "web" InfoS "todo: received some request"
    liftIO $ app req respond

makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Test env =
    runKatipT env $ liftIO $ createPool (DB.connect (envBoltCfg Test)) DB.close (envPool Test) 500 1
makePool Development env =
    runKatipT env $ liftIO $ createPool (DB.connect (envBoltCfg Development)) DB.close (envPool Development) 500 1
makePool Production env = do
    -- This function makes heavy use of the 'MaybeT' monad transformer, which
    -- might be confusing if you're not familiar with it. It allows us to
    -- combine the effects from 'IO' and the effect of 'Maybe' into a single
    -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
    -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
    -- give us a @Maybe a@, which would make the code quite a bit more
    -- verbose.
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        lift $ runKatipT env $ liftIO $ createPool (DB.connect (envBoltCfg Production)) DB.close (envPool Production) 500 1
    case pool of
        -- If we don't have a correct database configuration, we can't
        -- handle that in the program, so we throw an IO exception. This is
        -- one example where using an exception is preferable to 'Maybe' or
        -- 'Either'.
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just a -> return a

runDB :: (MonadReader ServerState m, MonadIO m) => DB.BoltActionT IO b -> m b
runDB query = do
    pool <- asks connectionPool
    liftIO $ withResource pool $ (flip DB.run) query

-- | The number of stripes to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

envBoltCfg :: Environment -> DB.BoltCfg
envBoltCfg Test        = def {DB.user = "neo4j", DB.password = "neo4j"}
envBoltCfg Development = def {DB.user = "neo4j", DB.password = "neo4j"}
envBoltCfg Production  = def {DB.user = "neo4j", DB.password = "prod"}