{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.Metrics       as M
import           Lens.Micro
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Metrics
import           System.Environment          (lookupEnv)
import           System.Metrics              (newStore)
import           System.Remote.Monitoring    (forkServer, serverMetricStore)
import           Api                         (app)
import           ServerState                 (ServerState (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (defaultLogEnv)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    env        <- lookupSetting "ENV" Development
    port       <- lookupSetting "PORT" 8081
    logEnv     <- defaultLogEnv
    pool       <- makePool env logEnv
    store      <- serverMetricStore <$> forkServer "localhost" 8000
    waiMetrics <- registerWaiMetrics store
    metr       <- M.initializeWith store
    let serverState = ServerState env metr logEnv poll
    let logger = setLogger env
    run port $ logger $ metrics waiMetrics $ app serverState

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
