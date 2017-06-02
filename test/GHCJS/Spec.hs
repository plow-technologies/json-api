module GHCJS.Spec (
   spec
 ) where

import ArbitraryInstances ()
import Data.JSONAPI
import Data.Proxy
import Test.Hspec
import Test.Hspec.Json.GHCJS

spec :: Spec
spec = do
  genericJSValToAeson   (Proxy :: Proxy Meta)
  genericAesonToJSVal   (Proxy :: Proxy Meta)
  genericJSValRoundtrip (Proxy :: Proxy Meta)
  genericAesonRoundtrip (Proxy :: Proxy Meta)