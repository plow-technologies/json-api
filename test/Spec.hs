{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

import           Data.JSONAPI.Identifier
import           Data.JSONAPI.Link
import           Data.JSONAPI.Meta
import           Data.JSONAPI.Relationship
import           Data.JSONAPI.Resource

import           Data.Proxy
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import           GHC.Generics (Generic)

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck

encodingAndDecodingSpec :: forall a. (Show a, Eq a, FromJSON a, ToJSON a) => Text -> a -> Spec
encodingAndDecodingSpec jsonText haskellValue =
  describe "JSON string decoding and Haskell value encoding" $ 
    it "be the same" $ do 
      decoded `shouldBe` (Just haskellValue)
      -- encoded `shouldBe` jsonLazyByteString
  where
    jsonLazyByteString = TL.encodeUtf8 . TL.fromStrict $ jsonText
    decoded = decode jsonLazyByteString
    -- encoded = encode haskellValue

main :: IO ()
main = do
  {-
  let x = decode $ TL.encodeUtf8 metaText :: Maybe Meta
  let z = encode metaPaginationExample
  print $ z == (TL.encodeUtf8 metaText)
  print $ x == Just metaPaginationExample
  print z
  print x
  -}
  hspec $ do
    encodingAndDecodingSpec metaText metaPaginationExample
    encodingAndDecodingSpec identifierText identifierExample
    encodingAndDecodingSpec identifierWithMetaText identifierWithMetaExample
    encodingAndDecodingSpec relationshipText relationshipExample
    encodingAndDecodingSpec resourceText resourceExample
    encodingAndDecodingSpec resourceWithLinksText resourceWithLinksExample
    roundtripSpecs (Proxy :: Proxy Links)
    roundtripSpecs (Proxy :: Proxy Identifier)
    roundtripSpecs (Proxy :: Proxy Meta)
    roundtripSpecs (Proxy :: Proxy Relationship)
    roundtripSpecs (Proxy :: Proxy (Resource User))

instance Arbitrary Meta where
  arbitrary = Meta <$> (HM.fromList <$> arbitrary)

instance Arbitrary Identifier where
  arbitrary = Identifier <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Links where
  arbitrary = do 
    i     <- choose (0,10)
    links <- vector i :: Gen [(Text,Text)]
    return $ Links (HM.fromList links)

instance Arbitrary Relationship where
  arbitrary = Relationship <$> arbitrary <*> arbitrary

instance Arbitrary Relationships where
  arbitrary =  do
    i             <- choose (0,10)
    relationships <- vector i :: Gen [(Text,Relationship)]
    return $ Relationships (HM.fromList relationships)
        
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Value where
  arbitrary = do 
    i  <- choose (0,10)
    keys   <- vector i :: Gen [Text]
    values <- fmap String <$> vector i :: Gen [Value]
    return $ Object $ HM.fromList $ zip keys values

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary
  
instance Arbitrary a => Arbitrary (Resource a) where
  arbitrary = Resource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

metaText :: Text
metaText = "{\"pagination\":{\"currentPage\":1,\"totalPages\":15}}"

metaPaginationExample :: Meta
metaPaginationExample = mkMeta $ Pagination 1 15

identifierText :: Text
identifierText = "{\"id\":\"1\",\"type\":\"user\"}"

identifierExample :: Identifier
identifierExample = Identifier "1" "user" Nothing

identifierWithMetaText :: Text
identifierWithMetaText = "{\"id\":\"1\",\"meta\":{\"pagination\":{\"currentPage\":1,\"totalPages\":15}},\"type\":\"user\"}"

identifierWithMetaExample :: Identifier
identifierWithMetaExample = Identifier "1" "user" (Just metaPaginationExample)

linksText :: Text
linksText = "{\"self\":\"/api/users/1\",\"next\":\"/api/users/2\"}"
  
linksExample :: Links
linksExample = Links $ HM.fromList [("self","/api/users/1"),("next","/api/users/2")]
  
relationshipText :: Text
relationshipText = "{\"data\":{\"id\":\"1\",\"type\":\"user\"},\"links\":{\"next\":\"/api/users/2\",\"self\":\"/api/users/1\"}}"

relationshipExample :: Relationship
relationshipExample = Relationship (Just identifierExample) (Just linksExample)

--resourceText :: Text
--resourceText = "{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userId\":2,\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\"},\"links\":{\"self\":\"/api/users/2\",\"friend\":\"/api/users/3\"},\"relationships\":[{}]}"

resourceText :: Text
resourceText = "{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userId\":2,\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\"}}"

resourceWithLinksText :: Text
resourceWithLinksText = "{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userId\":2,\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\"},\"links\":{\"self\":\"/api/users/2\",\"friend\":\"/api/users/3\"}}"

resourceExample :: Resource User
resourceExample =
  Resource
    (Identifier "2" "users" Nothing)
    (User 2 "Julio" "222 W. 22nd St")
    Nothing
    Nothing
    --(Just $ Links $ HM.fromList [("self","/api/users/2"),("friend","/api/users/3")])
    --(Just $ Relationships $ HM.fromList [("friend", Relationship (Just $ Identifier "3" "users" Nothing) (Just $ Links $ HM.fromList [("self","/api/users/3")]))])

resourceWithLinksExample :: Resource User
resourceWithLinksExample =
  Resource
    (Identifier "2" "users" Nothing)
    (User 2 "Julio" "222 W. 22nd St")
    (Just $ Links $ HM.fromList [("self","/api/users/2"),("friend","/api/users/3")])
    Nothing

data Pagination = 
  Pagination
    { currentPage :: Int
    , totalPages :: Int
    } deriving (Eq,Read,Show)

instance ToJSON Pagination where
  toJSON (Pagination c t) =
    object
      [ "currentPage" .= c
      , "totalPages"  .= t
      ]
      
instance MetaObject Pagination where
  typeName _ = "pagination"
  
data User =
  User
    { userId      :: Int
    , userName    :: Text
    , userAddress :: Text
    } deriving (Eq,Generic,Read,Show)
    
instance ToJSON User
instance FromJSON User