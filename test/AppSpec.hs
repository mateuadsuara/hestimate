{-# LANGUAGE OverloadedStrings #-}
module AppSpec(spec) where

import Lib (app, Estimation(..))

import Data.Aeson
import Data.ByteString.Lazy
import qualified Codec.Binary.UTF8.String as C
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai

toS :: ByteString -> String
toS = C.decode . unpack

bodyBeing :: ByteString -> ResponseMatcher
bodyBeing expected = let status = 200
                         match _ body | body == expected = Nothing
                                      | otherwise = Just $ "Got: " ++ toS body ++ ", but expected: " ++ (toS expected)
                     in status {matchBody = MatchBody match}

shouldHaveBody :: WaiSession st SResponse -> ByteString -> WaiExpectation st
shouldHaveBody r e = r `shouldRespondWith` bodyBeing e

spec :: Spec
spec = with (return app) $
    describe "GET /estimations" $ do
        it "responds with 200" $
            get "/estimations" `shouldRespondWith` 200
        it "responds with [Estimation]" $
            get "/estimations" `shouldHaveBody` encode [ Estimation 1
                                                       , Estimation 2
                                                       ]
