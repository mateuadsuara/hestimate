{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    , Estimation (..)
    ) where

import Data.ByteString (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple
import Control.Exception (bracket)

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class

data Estimation = Estimation
  { id :: Int
  } deriving (Eq, Show)

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
server = liftIO estimations :<|> estimation

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

estimations :: IO [Estimation]
estimations = do
  conn <- getConnection
  result <- query_ conn "select id from hestimates"
  let estimates = map (Estimation . fromOnly) result
  close conn
  return estimates

insertEstimation :: IO Estimation
insertEstimation = do
  conn <- getConnection
  result <- query_ conn "insert into hestimates(id) values (5) returning id"
  let [estimate] = map (Estimation . fromOnly) result
  close conn
  return estimate

estimation :: Estimation -> Handler Estimation
estimation (Estimation id) = liftIO insertEstimation

