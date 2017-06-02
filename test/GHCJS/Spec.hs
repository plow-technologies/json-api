module GHCJS.Spec (
   spec
 ) where

import ArbitraryInstances ()
-- import Data.JSONAPI
import Data.Proxy
import Test.Hspec
import Test.Hspec.Json.GHCJS

import Data.Aeson

spec :: Spec
spec = do
  genericJSValToAeson   (Proxy :: Proxy Object)
  genericAesonToJSVal   (Proxy :: Proxy Object)
  genericJSValRoundtrip (Proxy :: Proxy Object)
  genericAesonRoundtrip (Proxy :: Proxy Object)
{-
  genericJSValToAeson   (Proxy :: Proxy Meta)
  genericAesonToJSVal   (Proxy :: Proxy Meta)
  genericJSValRoundtrip (Proxy :: Proxy Meta)
  genericAesonRoundtrip (Proxy :: Proxy Meta)
-}