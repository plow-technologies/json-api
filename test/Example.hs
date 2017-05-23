{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import           ArbitraryInstances
import           Control.Arrow ((&&&))
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.JSONAPI.Document
import           Data.JSONAPI.Identifier
import           Data.JSONAPI.Link
import           Data.JSONAPI.Meta
import           Data.JSONAPI.Relationship
import           Data.JSONAPI.Resource
import qualified Data.Text as T
import           Data.Text (Text)

import           GHC.Generics (Generic)

import           Test.QuickCheck


documentText :: Text
documentText = "{\"data\":{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\",\"userId\":2},\"links\":{\"self\":\"/api/users/2\"}}}"

documentExample :: Document User
documentExample =
  Document
    [toResource userExample]
    Nothing
    Nothing 
    []

documentMultiResourceText :: Text
documentMultiResourceText = 
  "{\"data\":[{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\",\"userId\":2},\"links\":{\"self\":\"/api/users/2\"}}"
         <> ",{\"id\":\"3\",\"type\":\"users\",\"attributes\":{\"userName\":\"Jordi\",\"userAddress\":\"333 W. 33rd St\",\"userId\":3},\"links\":{\"self\":\"/api/users/3\"}}]}"

documentMultiResourceExample :: Document User
documentMultiResourceExample =
  Document
    [toResource userExample, toResource user2Example]
    Nothing
    Nothing 
    []

userExample :: User
userExample =
  User 2 "Julio" "222 W. 22nd St"

user2Example :: User
user2Example =
  User 3 "Jordi" "333 W. 33rd St"

documentGroupResourceText :: Text
documentGroupResourceText = 
  "{\"data\":{\"attributes\":{\"groupId\":1,\"groupName\":\"test-group\"},\"relationships\":{\"members\":{\"data\":{\"id\":\"2\",\"type\":\"users\"},\"links\":{\"self\":\"/api/users/2\"}}},\"id\":\"1\",\"type\":\"groups\",\"links\":{\"self\":\"/api/groups/1\"}}}"

documentGroupResourceExample :: Document GroupResource
documentGroupResourceExample = 
  Document
    [toResource groupResourceExample]
    Nothing 
    Nothing 
    []

groupExample :: Group
groupExample = Group 1 "test-group"

groupResourceExample :: GroupResource
groupResourceExample = GroupResource groupExample [userExample]

groupResourceResourceText :: Text
groupResourceResourceText = 
     "{\"attributes\":{\"groupId\":1,\"groupName\":\"test-group\"}"
  <> ",\"relationships\":{\"members\":{\"data\":{\"id\":\"2\",\"type\":\"users\"}"
  <> ",\"links\":{\"self\":\"/api/users/2\"}}},\"id\":\"1\",\"type\":\"groups\"}"

groupResourceResourceExample :: Resource GroupResource
groupResourceResourceExample = 
  Resource
    (Identifier "1" "groups" Nothing)
    (GroupResource (Group 1 "test-group") [userExample])
    Nothing
    (Just $ Relationships $ HM.fromList [("members", Relationship (Just $ Identifier "2" "users" Nothing) (Just $ Links $ HM.fromList [("self", (LinkHref "/api/users/2"))]))])

recodedGroupResourceResourceExample :: Resource GroupResource
recodedGroupResourceResourceExample = 
  Resource
    (Identifier "1" "groups" Nothing)
    (GroupResource (Group 1 "test-group") [])
    Nothing
    (Just $ Relationships $ HM.fromList [("members", Relationship (Just $ Identifier "2" "users" Nothing) (Just $ Links $ HM.fromList [("self", (LinkHref "/api/users/2"))]))])

{-
Resource 
  { identifier    :: Identifier
  , resource      :: a
  , links         :: Maybe Links
  , relationships :: Maybe Relationships
-}
  
metaText :: Text
metaText = "{\"pagination\":{\"currentPage\":1,\"totalPages\":15}}"

metaPaginationExample :: Meta
metaPaginationExample = mkMeta $ Pagination 1 15

identifierText :: Text
identifierText = "{\"id\":\"1\",\"type\":\"users\"}"

identifierExample :: Identifier
identifierExample = Identifier "1" "users" Nothing

identifierWithMetaText :: Text
identifierWithMetaText = "{\"id\":\"1\",\"meta\":{\"pagination\":{\"currentPage\":1,\"totalPages\":15}},\"type\":\"users\"}"

identifierWithMetaExample :: Identifier
identifierWithMetaExample = Identifier "1" "users" (Just metaPaginationExample)

linksText :: Text
linksText = "{\"self\":\"/api/users/1\",\"next\":\"/api/users/2\"}"
  
linksExample :: Links
linksExample = Links $ HM.fromList [("self",(LinkHref "/api/users/1")),("next",(LinkHref "/api/users/2"))]
  
relationshipText :: Text
relationshipText = "{\"data\":{\"id\":\"1\",\"type\":\"users\"},\"links\":{\"next\":\"/api/users/2\",\"self\":\"/api/users/1\"}}"

relationshipExample :: Relationship
relationshipExample = Relationship (Just identifierExample) (Just linksExample)

--resourceText :: Text
--resourceText = "{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userId\":2,\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\"},\"links\":{\"self\":\"/api/users/2\",\"friend\":\"/api/users/3\"},\"relationships\":[{}]}"

resourceText :: Text
resourceText = 
     "{\"id\":\"2\",\"type\":\"users\""
  <> ",\"attributes\":{\"userId\":2,\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\"}"
  <> ",\"relationships\":{\"friend\":{\"data\":{\"id\":\"3\",\"type\":\"users\"},\"links\":{\"self\":\"/api/users/3\"}}}"
  <> "}"

resourceWithLinksText :: Text
resourceWithLinksText = "{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userId\":2,\"userName\":\"Julio\",\"userAddress\":\"222 W. 22nd St\"},\"links\":{\"self\":\"/api/users/2\",\"friend\":\"/api/users/3\"}}"

resourceExample :: Resource User
resourceExample =
  Resource
    (Identifier "2" "users" Nothing)
    (User 2 "Julio" "222 W. 22nd St")
    Nothing
    (Just $ Relationships $ HM.fromList [("friend", Relationship (Just $ Identifier "3" "users" Nothing) (Just $ Links $ HM.fromList [("self", (LinkHref "/api/users/3"))]))])
    --(Just $ Links $ HM.fromList [("self","/api/users/2"),("friend","/api/users/3")])
    --(Just $ Relationships $ HM.fromList [("friend", Relationship (Just $ Identifier "3" "users" Nothing) (Just $ Links $ HM.fromList [("self","/api/users/3")]))])

resourceWithLinksExample :: Resource User
resourceWithLinksExample =
  Resource
    (Identifier "2" "users" Nothing)
    userExample
    (Just $ Links $ HM.fromList [("self",(LinkHref "/api/users/2")),("friend",(LinkHref "/api/users/3"))])
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

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary
  
data Group =
  Group 
    { groupId   :: Int
    , groupName :: Text
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Group
instance FromJSON Group

instance Arbitrary Group where
  arbitrary = Group <$> arbitrary <*> arbitrary
  
instance ResourcefulEntity User where
  resourceIdentifier      = T.pack . show . userId
  resourceType            = const "users"
  resourceLinks      user = Just $ mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show $ userId user)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const Nothing

instance ResourcefulEntity Group where
  resourceIdentifier      = T.pack . show . groupId
  resourceType            = const "groups"
  resourceLinks     group = Just $ mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show $ groupId group)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const Nothing

data GroupResource =
  GroupResource 
    { grGroup :: Group
    , grUsers :: [User]
    } deriving (Eq, Generic, Read, Show)

instance ToJSON GroupResource where
  toJSON (GroupResource g _us) = toJSON g
  
instance FromJSON GroupResource where
  parseJSON o = --  withObject "GroupResource" $ \o ->
    GroupResource <$> parseJSON o <*> pure []

instance Arbitrary GroupResource where
  arbitrary = do
    i <- choose (1,5)
    GroupResource <$> arbitrary <*> vector i

instance ResourcefulEntity GroupResource where
  resourceIdentifier       = T.pack . show . groupId . grGroup
  resourceType             = const "groups"
  resourceLinks         gr = Just $ mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show . groupId . grGroup $ gr)))]
  resourceMetaData         = const Nothing
  resourceRelationships gr = Just . Relationships . HM.fromList $ (const "members" &&& id) . (uncurry Relationship) . (Just . mkIdentifier &&& resourceLinks) <$> grUsers gr
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) Nothing
  toResource gr =
    Resource
      (Identifier (resourceIdentifier gr) (resourceType gr) (resourceMetaData gr))
      (gr { grUsers = [] } )
      (resourceLinks gr)
      (resourceRelationships gr)
{-
fromResource :: Resource a -> a
fromResource = resource

toResource :: a -> Resource a
toResource a =
  Resource
    (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
    a
    (resourceLinks a)
    (resourceRelationships a)

class (ToJSON a, FromJSON a) => ResourcefulEntity a where
  resourceIdentifier    :: a -> Text
  resourceType          :: a -> Text
  resourceLinks         :: a -> Maybe Links
  resourceMetaData      :: a -> Maybe Meta
  resourceRelationships :: a -> Maybe Relationships

  fromResource :: Resource a -> a
  fromResource = resource

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)

-}