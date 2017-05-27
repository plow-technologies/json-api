{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Aeson

import           Data.JSONAPI.Document
import           Data.JSONAPI.Identifier
import           Data.JSONAPI.Link
import           Data.JSONAPI.Meta
import           Data.JSONAPI.Relationship
import           Data.JSONAPI.Resource

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Typeable


import           Test.Aeson.GenericSpecs
import           Test.Hspec

import           Examples
import           Types

-- given a JSON string and a value that has derived FromJSON and ToJSON, attempt
-- to create a Haskell Value from the JSON String and see if it matches the 
-- Haskell value
jsonTextHaskellValuePairSpec :: forall a. (Typeable a, Show a, Eq a, FromJSON a) => Text -> a -> Spec
jsonTextHaskellValuePairSpec jsonText haskellValue =
  describe ("JSON String and matching Haskell Value (" ++ (show (typeRep (Proxy :: Proxy a))) ++ ")") $ 
    it "decoding JSON String should be equal to Haskell Value" $
      decoded `shouldBe` (Just haskellValue)
  where
    jsonLazyByteString = TL.encodeUtf8 . TL.fromStrict $ jsonText
    decoded = decode jsonLazyByteString


main :: IO ()
main = do
  hspec $ do
    jsonTextHaskellValuePairSpec documentGroupResourceText documentGroupResourceExample    
    {-
    jsonTextHaskellValuePairSpec documentText documentExample
    jsonTextHaskellValuePairSpec documentMultiResourceText documentMultiResourceExample
    jsonTextHaskellValuePairSpec documentGroupResourceText documentGroupResourceExample
    jsonTextHaskellValuePairSpec documentUserResourceText documentUserResourceExample
    jsonTextHaskellValuePairSpec identifierText identifierExample
    jsonTextHaskellValuePairSpec identifierWithMetaText identifierWithMetaExample

    jsonTextHaskellValuePairSpec metaText metaPaginationExample
    jsonTextHaskellValuePairSpec relationshipText relationshipExample
    jsonTextHaskellValuePairSpec resourceText resourceExample
    jsonTextHaskellValuePairSpec resourceWithLinksText resourceWithLinksExample
    
    -- run serialization tests on arbitrary values

    roundtripSpecs (Proxy :: Proxy (Document Group))
    roundtripSpecs (Proxy :: Proxy (Document User))
    roundtripSpecs (Proxy :: Proxy Identifier)
    roundtripSpecs (Proxy :: Proxy Links)
    roundtripSpecs (Proxy :: Proxy Meta)
    roundtripSpecs (Proxy :: Proxy Relationship)
    roundtripSpecs (Proxy :: Proxy (Resource Group))    
    roundtripSpecs (Proxy :: Proxy (Resource User))
    -}