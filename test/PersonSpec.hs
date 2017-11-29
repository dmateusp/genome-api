{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module PersonSpec where

import           System.Environment          (lookupEnv)
import           Server                      (app)
import           ServerState                 (ServerState (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (defaultLogEnv)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Network.HTTP.Types.Header
import           Api.Person
import           Data.Aeson                  (encode)
spec :: Spec
spec = with appInstance $ do
  describe "/persons" $ do
      it "returns some persons" $ do
        get "persons" `shouldRespondWith` personsResponseEmpty

personsResponseEmpty =
  let ResponseMatcher status headers body = [json|[]|]
  in ResponseMatcher status [hContentType <:> "application/json;charset=utf-8"] body

appInstance :: IO Application
appInstance = do
         let port = 8083
         let env  = Test
         logEnv <- defaultLogEnv
         pool   <- makePool env logEnv
         let serverState = ServerState Test logEnv pool
         let logger = setLogger Test
         return $ app serverState
