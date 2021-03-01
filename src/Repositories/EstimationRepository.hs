{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Repositories.EstimationRepository(EstimationRepository(..), PostgresEstimation, InMemoryEstimation) where

import Model.Estimation ( Estimation(Estimation) )

import Data.Pool
import Control.Monad.State ( MonadState(get), modify, State )
import Control.Monad.Reader ( ReaderT, ask, liftIO )
import Database.PostgreSQL.Simple (query, query_, fromOnly, Connection, connectPostgreSQL, close)

import Control.Concurrent.MVar ( MVar, readMVar, takeMVar, putMVar )

class Monad m => EstimationRepository m where
  retrieveEstimations :: m [Estimation]
  insertEstimation :: Estimation -> m Estimation

connStr = "dbname='hestimate' host='localhost' user='root' password='ee3a7fb68de968d24'"

getConnection :: IO Connection
getConnection = connectPostgreSQL connStr

type PostgresEstimation = ReaderT (Pool Connection) IO

--TODO: Generar el ID con un UUID

instance EstimationRepository PostgresEstimation where
  retrieveEstimations = do
    pool <- ask
    liftIO $ withResource pool $ \conn -> do
      result <- query_ conn "select id from hestimates"
      let estimates = map (Estimation . fromOnly) result
      return estimates

  insertEstimation (Estimation id) = do
    pool <- ask
    liftIO $ withResource pool $ \conn -> do
      result <- query conn "insert into hestimates(id) values (?) returning id" [id]
      let [estimate] = map (Estimation . fromOnly) result
      return estimate

type InMemoryEstimation = ReaderT (MVar [Estimation]) IO

instance EstimationRepository InMemoryEstimation where
  retrieveEstimations = do
    mvar <- ask
    liftIO $ readMVar mvar

  insertEstimation e = do
    mvar <- ask
    liftIO $ do
      estimations <- takeMVar mvar
      let estimations' = e:estimations
      putMVar mvar estimations'
    return e
