{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    , Estimation (..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Estimation = Estimation
  { id :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Estimation)

type API
  =    "estimations" :> Get '[JSON] [Estimation]
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
  return estimations :<|> estimation

estimations :: [Estimation]
estimations = [ Estimation 1
              , Estimation 2
              ]

estimation :: Estimation -> Handler Estimation
estimation (Estimation id) = return (Estimation $ id + 5) 

