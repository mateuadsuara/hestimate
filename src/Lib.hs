{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data Estimation = Estimation
  { id :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Estimation)

type API
  =    "users"       :> Get '[JSON] [User]
  :<|> "estimations" :> Get '[JSON] [Estimation]
  :<|> "estimations" :> ReqBody '[JSON] Estimation :> Post '[JSON] Estimation

startApp :: IO ()
startApp = do
  putStrLn "Listening on port 8080..."
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  return users :<|> return estimations :<|> estimation

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

estimations :: [Estimation]
estimations = [ Estimation 1
              , Estimation 2
              ]

estimation :: Estimation -> Handler Estimation
estimation (Estimation id) = return (Estimation $ id + 5) 

