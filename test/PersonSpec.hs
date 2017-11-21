{-# LANGUAGE OverloadedStrings #-}

module PersonSpec where

import           System.Environment          (lookupEnv)
import           Server                      (app)
import           ServerState                 (ServerState (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (defaultLogEnv)
import           Safe                        (readMay)
import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)

spec :: Spec
spec = with appInstance $ do
  describe "/persons" $ do
      it "returns an empty list" $ do
        get "persons" `shouldRespondWith` 200

appInstance :: IO Application
appInstance = do
         let port = 8083
         let env  = Test
         logEnv <- defaultLogEnv
         pool   <- makePool env logEnv
         let serverState = ServerState Test logEnv pool
         let logger = setLogger Test
         return $ app serverState
