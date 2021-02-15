{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes       #-}

module Lib
    ( startApp
    , app
    , inMemoryToHandler
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
    ( EstimationRepository(..), PostgresEstimation, InMemoryEstimation)
import Control.Monad.State (evalState)

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
  run 8080 (app postgresToHandler)

app :: EstimationRepository m => RepoToHandler m -> Application
app repoToHandler = serve api (hoistServer api repoToHandler server)  

api :: Proxy API
api = Proxy

postgresToHandler :: PostgresEstimation a -> Handler a
postgresToHandler = liftIO 

inMemoryToHandler :: InMemoryEstimation a -> Handler a
inMemoryToHandler m = return (evalState m [ Estimation 1, Estimation 2]) 

type RepoToHandler m = forall a . m a -> Handler a

server :: EstimationRepository m => ServerT API m
server = retrieveEstimations :<|> insertEstimation

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