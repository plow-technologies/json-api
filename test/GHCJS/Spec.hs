{-# LANGUAGE OverloadedStrings #-}
module GHCJS.Spec (
   spec
 ) where

import ArbitraryInstances ()
import Data.JSONAPI
--import Data.Proxy
import Test.Hspec
--import Test.Hspec.Json.GHCJS

import Data.Text (Text)
import Data.Aeson
import qualified Data.HashMap.Strict as HS

import GHCJS.Marshal


spec :: Spec
spec = do
  describe "ToJSVal/FromJSVal Object" $
    it "commute" $ do
      let val = HS.fromList [("hi",Number 1),("bye",Number 2),("aloha",Number 3)] :: HS.HashMap Text Value
      jsVal <- toJSVal val
      mJsValToHsVal <- fromJSVal jsVal :: IO (Maybe (HS.HashMap Text Value))
      (Just val) `shouldBe` mJsValToHsVal
  
  describe "ToJSVal/FromJSVal Meta" $
    it "commute" $ do
      let val = Meta $ HS.fromList [("hi",Number 1),("bye",Number 2),("aloha",Number 3)] :: Meta
      jsVal <- toJSVal val
      mJsValToHsVal <- fromJSVal jsVal :: IO (Maybe Meta)
      (Just val) `shouldBe` mJsValToHsVal
  
-- this currently won't pass because FromJSVal Data.Aeson.Types.Value is broken
{-
  genericJSValToAeson   (Proxy :: Proxy Object)
  genericAesonToJSVal   (Proxy :: Proxy Object)
  genericJSValRoundtrip (Proxy :: Proxy Object)
  genericAesonRoundtrip (Proxy :: Proxy Object)

  genericJSValToAeson   (Proxy :: Proxy Meta)
  genericAesonToJSVal   (Proxy :: Proxy Meta)
  genericJSValRoundtrip (Proxy :: Proxy Meta)
  genericAesonRoundtrip (Proxy :: Proxy Meta)
-}