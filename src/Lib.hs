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
import Network.Wai.Middleware.Cors (simpleCors, corsMethods, cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Model.Estimation

import Repositories.EstimationRepository
    ( EstimationRepository(..), PostgresEstimation, InMemoryEstimation)
import Control.Monad.Reader (runReaderT)

import Control.Concurrent.MVar ( MVar, readMVar, takeMVar, putMVar )

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
  run 8080 (app $ postgresToHandler pool)

corsConfig :: Middleware
corsConfig = cors (const $ Just simpleCorsResourcePolicy { 
  corsMethods = ["GET", "HEAD", "POST", "OPTIONS"] 
  , corsRequestHeaders = ["content-type"]
})

app :: EstimationRepository m => RepoToHandler m -> Application
app repoToHandler = corsConfig $ serve api (hoistServer api repoToHandler server)

api :: Proxy API
api = Proxy

postgresToHandler :: Pool Connection -> PostgresEstimation a -> Handler a
postgresToHandler conn m = liftIO $ runReaderT m conn

inMemoryToHandler :: MVar [Estimation] -> InMemoryEstimation a -> Handler a
inMemoryToHandler mvar m = liftIO $ runReaderT m mvar

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
