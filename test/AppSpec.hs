{-# LANGUAGE OverloadedStrings #-}
module AppSpec(spec) where

import Lib (app, inMemoryToHandler)

import Data.Aeson
import Data.ByteString.Lazy
import qualified Codec.Binary.UTF8.String as C
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai
    ( get,
      request,
      shouldRespondWith,
      with,
      WaiExpectation,
      WaiSession,
      MatchBody(MatchBody),
      ResponseMatcher(matchBody) ) 
import Network.HTTP.Types(methodPost,hContentType )      

import Model.Estimation ( Estimation(Estimation) )

import Control.Monad.Reader ( liftIO )
import Control.Concurrent.MVar ( MVar, newMVar )

toS :: ByteString -> String
toS = C.decode . unpack

bodyBeing :: ByteString -> ResponseMatcher
bodyBeing expected = let status = 200
                         match _ body | body == expected = Nothing
                                      | otherwise = Just $ "Got: " ++ toS body ++ ", but expected: " ++ (toS expected)
                     in status {matchBody = MatchBody match}

shouldHaveBody :: WaiSession st SResponse -> ByteString -> WaiExpectation st
shouldHaveBody r e = r `shouldRespondWith` bodyBeing e

initialList :: [Estimation]
initialList = [Estimation 1, Estimation 2]

initApp :: IO Application
initApp = do
    mvar <- liftIO $ newMVar initialList
    return (app $ inMemoryToHandler mvar)

spec :: Spec
spec =
    with initApp $
        describe "GET /estimations" $ do
            it "responds with 200" $
                get "/estimations" `shouldRespondWith` 200
            it "responds with [Estimation]" $
                get "/estimations" `shouldHaveBody` encode initialList
            it "add new elements" $ do
                request methodPost "/estimations" [(hContentType, "application/json")] "{\"id\":98}" 
                    `shouldHaveBody` encode (Estimation 98)
                request methodPost "/estimations" [(hContentType, "application/json")] "{\"id\":99}" 
                    `shouldHaveBody` encode (Estimation 99)
                get "/estimations" `shouldHaveBody` encode (Estimation 99:Estimation 98:initialList)