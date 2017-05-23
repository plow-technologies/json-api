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

-- import qualified Data.JSONAPI.MetaSpec as M

import           Data.Proxy
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Typeable

import           Example

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck

jsonTextHaskellValuePairSpec :: forall a. (Typeable a, Show a, Eq a, FromJSON a, ToJSON a) => Text -> a -> Spec
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
    jsonTextHaskellValuePairSpec documentText documentExample
    jsonTextHaskellValuePairSpec documentMultiResourceText documentMultiResourceExample
    jsonTextHaskellValuePairSpec documentGroupResourceText documentGroupResourceExample
    jsonTextHaskellValuePairSpec identifierText identifierExample
    jsonTextHaskellValuePairSpec identifierWithMetaText identifierWithMetaExample

    jsonTextHaskellValuePairSpec metaText metaPaginationExample
    jsonTextHaskellValuePairSpec relationshipText relationshipExample
    jsonTextHaskellValuePairSpec resourceText resourceExample
    jsonTextHaskellValuePairSpec resourceWithLinksText resourceWithLinksExample

    -- jsonTextHaskellValuePairSpec groupResourceResourceText groupResourceResourceExample    
    describe "Resource" $
      it "should match" $ 
        decoded `shouldBe` (Just recodedGroupResourceResourceExample)
      where
        jsonLazyByteString = TL.encodeUtf8 . TL.fromStrict $ groupResourceResourceText
        decoded            = decode jsonLazyByteString
    {-
    roundtripSpecs (Proxy :: Proxy (Document GroupResource))    
    roundtripSpecs (Proxy :: Proxy (Document User))
    roundtripSpecs (Proxy :: Proxy Identifier)
    roundtripSpecs (Proxy :: Proxy Links)
    roundtripSpecs (Proxy :: Proxy Meta)
    roundtripSpecs (Proxy :: Proxy Relationship)
    roundtripSpecs (Proxy :: Proxy (Resource User))
    -}