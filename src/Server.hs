module Server(startApp) where

import Servant
import Api
import Network.Wai as W
import Network.Wai.Handler.Warp as W

type Port = Int
startApp :: Server.Port -> IO ()
startApp p = run p app

  where app :: Application
        app = serve api server

        api :: Proxy Api
        api = Proxy