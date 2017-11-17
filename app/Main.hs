{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.Metrics       as M
import           Lens.Micro
import           Network.Wai.Handler.Warp    (run)
import qualified Network.Wai.Metrics         as WMetrics
import           System.Environment          (lookupEnv)
import           System.Metrics              (newStore)
import           System.Remote.Monitoring    (forkServer, serverMetricStore)
import           Server                      (app)
import           ServerState                 (ServerState (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (defaultLogEnv)
import           Safe                        (readMay)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    env        <- lookupSetting "ENV" Development
    port       <- lookupSetting "PORT" 8081
    logEnv     <- defaultLogEnv
    pool       <- makePool env logEnv
    store      <- serverMetricStore <$> forkServer "localhost" 8000
    waiMetrics <- WMetrics.registerWaiMetrics store
    metr       <- M.initializeWith store
    let serverState = ServerState env metr logEnv pool
    let logger = setLogger env
    run port $ logger $ WMetrics.metrics waiMetrics $ app serverState

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
