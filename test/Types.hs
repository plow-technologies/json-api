{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where
  
import           ArbitraryInstances ()
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
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

-- A collection of sample types for testing
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
  resourceLinks      user = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show $ userId user)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty

instance ResourcefulEntity Group where
  resourceIdentifier      = T.pack . show . groupId
  resourceType            = const "groups"
  resourceLinks     group = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show $ groupId group)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty

-- These resource types are used to associate one type with its relationships 
-- The ToJSON and FromJSON should ignore the relationships
-- ResourcefulEntity toResource should also set the resources to empty list or nothing

data UserResource =
  UserResource 
    { urUser    :: User
    , urFriends :: [User]
    , urBoss    :: Maybe User
    } deriving (Eq, Generic, Read, Show)

instance ToJSON UserResource where
  toJSON (UserResource u _friends _boss) = toJSON u

instance FromJSON UserResource where
  parseJSON o =
    UserResource <$> parseJSON o <*> pure [] <*> pure Nothing
    
instance ResourcefulEntity UserResource where
  resourceIdentifier       = T.pack . show . userId . urUser
  resourceType             = const "users"
  resourceLinks         ur = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show . userId . urUser $ ur)))]
  resourceMetaData         = const Nothing
  resourceRelationships ur = Relationships . HM.fromList $ friends ++ boss
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) Nothing
      friends = [("friends", (Relationship (mkIdentifier <$> urFriends ur) emptyLinks))]
      boss    = 
        case urBoss ur of
          Nothing  -> []
          Just bss -> [("boss", (Relationship [mkIdentifier bss] emptyLinks))]
  toResource ur =
    Resource
      (Identifier (resourceIdentifier ur) (resourceType ur) (resourceMetaData ur))
      (ur { urFriends = [], urBoss = Nothing } )
      (resourceLinks ur)
      (resourceRelationships ur)

data GroupResource =
  GroupResource 
    { grGroup :: Group
    , grUsers :: [User]
    } deriving (Eq, Generic, Read, Show)

instance ToJSON GroupResource where
  toJSON (GroupResource g _us) = toJSON g
  
instance FromJSON GroupResource where
  parseJSON o =
    GroupResource <$> parseJSON o <*> pure []

instance Arbitrary GroupResource where
  arbitrary = do
    i <- choose (1,5)
    GroupResource <$> arbitrary <*> vector i

instance ResourcefulEntity GroupResource where
  resourceIdentifier       = T.pack . show . groupId . grGroup
  resourceType             = const "groups"
  resourceLinks         gr = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show . groupId . grGroup $ gr)))]
  resourceMetaData         = const Nothing
  resourceRelationships gr = Relationships . HM.fromList $ [("members", (Relationship (mkIdentifier <$> grUsers gr) emptyLinks))]
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) Nothing
  toResource gr =
    Resource
      (Identifier (resourceIdentifier gr) (resourceType gr) (resourceMetaData gr))
      (gr { grUsers = [] } )
      (resourceLinks gr)
      (resourceRelationships gr)
      
mkGroupResourceDocument :: GroupResource -> Document GroupResource
mkGroupResourceDocument gr = Document [toResource gr] Nothing Nothing [members]
  where
    members = toJSON (toResource <$> grUsers gr) 


mkk :: Document GroupResource -> GroupResource
mkk dc = undefined -- fromResource (dc
  where
    -- memberRelationships = (\(Relationships r) -> lookup "members" r) <$> (resourceRelationships dc)
    documentIncluded = _included dc
    _rss = relationships <$> _data dc -- [Resource a]
    -- (\rs) _rss
    
    getUsers :: [Identifier] -> [Value] -> [User]
    getUsers is vs = fromResource <$> filter (\u -> (Data.JSONAPI.Resource.identifier u) `elem` is) users
      where
        users  = catMaybes $ resultToMaybe . fromJSON <$> vs :: [Resource User]
    -- getIdentifiers :: Resource a -> [Identifier]
    -- getIdentifiers
    _mems = (\r -> getUsers (getRelationshipIdentifiers "members" (relationships r)) documentIncluded) <$> _data dc


getRelationshipIdentifiers :: Text -> Relationships -> [Identifier]
getRelationshipIdentifiers t (Relationships rs) =
  case HM.lookup t rs of
    Nothing -> []
    Just r  -> identifiers r

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe x = 
  case x of
    Error _ -> Nothing
    Data.Aeson.Success a -> Just a

-- document has included
-- relationship has 
{-
documentUserResourceExample :: Document UserResource
documentUserResourceExample = 
  Document
    [toResource userResourceExample]
    Nothing 
    Nothing 
    []


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