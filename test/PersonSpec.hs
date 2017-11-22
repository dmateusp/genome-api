{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PersonSpec where

import           System.Environment          (lookupEnv)
import           Server                      (app)
import           ServerState                 (ServerState (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (defaultLogEnv)
import           Safe                        (readMay)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Api.Person
import           Data.Aeson                  (encode)
import           Data.ByteString.Lazy.Char8 as L (unpack)

spec :: Spec
spec = with appInstance $ do
  describe "/persons" $ do
      it "returns some persons" $ do
        get "persons" `shouldRespondWith` [json|[]|] where

appInstance :: IO Application
appInstance = do
         let port = 8083
         let env  = Test
         logEnv <- defaultLogEnv
         pool   <- makePool env logEnv
         let serverState = ServerState Test logEnv pool
         let logger = setLogger Test
         return $ app serverState