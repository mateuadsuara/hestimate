{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.ByteString (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple
    ( close, connectPostgreSQL, execute_, Connection )
import Control.Exception (bracket)

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class

import Model.Estimation

import Repositories.EstimationRepository

$(deriveJSON defaultOptions ''Estimation)

type DBConnectionString = ByteString

type API
  =    "estimations" :> Get '[JSON] [Estimation]
  :<|> "estimations" :> ReqBody '[JSON] Estimation :> Post '[JSON] Estimation

connStr = "dbname='hestimate' host='localhost' user='root' password='ee3a7fb68de968d24'"

getConnection :: IO Connection
getConnection = connectPostgreSQL connStr

startApp :: IO ()
startApp = do
  pool <- initConnectionPool connStr  
  initDB connStr
  putStrLn "Listening on port 8080..."
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO retrieveEstimations :<|> liftIO . insertEstimation

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool getConnection
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket getConnection close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS hestimates (id numeric primary key)"
  return ()