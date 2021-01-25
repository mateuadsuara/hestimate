{-# LANGUAGE OverloadedStrings #-}
module AppSpec(spec) where

import Lib (app, Estimation(..))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Aeson
import Network.HTTP.Types.Header
import Data.ByteString.Lazy
import qualified Codec.Binary.UTF8.String as C

main :: IO ()
main = hspec spec

toS :: ByteString -> String
toS = C.decode . unpack

withBody :: ByteString -> MatchBody
withBody expected = MatchBody $ \_ body -> if body == expected then Nothing else Just $ "Got: " ++ (toS body) ++ ", but expected: " ++ (toS expected)

spec :: Spec
spec = with (return app) $ do
    describe "GET /estimations" $ do
        it "responds with 200" $ do
            get "/estimations" `shouldRespondWith` 200
        it "responds with [Estimation]" $ do
            let
              estimations = encode
                [ Estimation 1
                , Estimation 3
                ]
              result = get "/estimations"
            result `shouldRespondWith` 200 {matchBody = withBody estimations}
