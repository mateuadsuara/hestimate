{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Repositories.EstimationRepository(EstimationRepository(..)) where 

import Model.Estimation
import Control.Monad.Reader
import Database.PostgreSQL.Simple (query, query_, fromOnly, Connection, connectPostgreSQL, close)

class Monad m => EstimationRepository m where
  retrieveEstimations :: m [Estimation]
  insertEstimation :: Estimation -> m Estimation
  
connStr = "dbname='hestimate' host='localhost' user='root' password='ee3a7fb68de968d24'"

getConnection :: IO Connection
getConnection = connectPostgreSQL connStr
  
type PostgresEstimation = IO

instance EstimationRepository PostgresEstimation where
  retrieveEstimations = do
    conn <- getConnection
    result <- query_ conn "select id from hestimates"
    let estimates = map (Estimation . fromOnly) result
    close conn
    return estimates

  insertEstimation (Estimation id) = do
    conn <- getConnection
    result <- query conn "insert into hestimates(id) values (?) returning id" [id]
    let [estimate] = map (Estimation . fromOnly) result
    close conn
    return estimate