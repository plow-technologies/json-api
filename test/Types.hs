{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where
  
import           ArbitraryInstances ()
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Monoid ((<>))
import           Data.JSONAPI hiding (Error(..))
import qualified Data.Text as T
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Test.QuickCheck

-- A collection of sample types for testing
data Pagination = 
  Pagination
    { currentPage :: Int
    , totalPages  :: Int
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
    } deriving (Eq,Generic,Read,Show)
    
instance ToJSON User
instance FromJSON User

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

data BlogPost =
  BlogPost 
    { blogPostId   :: Int
    , blogPostText :: Text
    } deriving (Eq,Generic,Read,Show)

instance ToJSON BlogPost
instance FromJSON BlogPost

instance Arbitrary BlogPost where
  arbitrary = BlogPost <$> arbitrary <*> arbitrary
  
data Group =
  Group 
    { groupId   :: Int
    , groupName :: Text
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Group
instance FromJSON Group

instance Arbitrary Group where
  arbitrary = Group <$> arbitrary <*> arbitrary
  
instance ResourceEntity User where
  resourceIdentifier      = T.pack . show . userId
  resourceType            = const "users"
  resourceLinks      user = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show $ userId user)))]
  resourceMetaData        = const metaEmpty
  resourceRelationships   = const $ Relationships HM.empty

instance ResourceEntity BlogPost where
  resourceIdentifier      = T.pack . show . blogPostId
  resourceType            = const "blogposts"
  resourceLinks      user = mkLinks [("self", LinkHref ("/api/blog/post/" <> (T.pack . show $ blogPostId user)))]
  resourceMetaData        = const metaEmpty
  resourceRelationships   = const $ Relationships HM.empty

instance ResourceEntity Group where
  resourceIdentifier      = T.pack . show . groupId
  resourceType            = const "groups"
  resourceLinks     group = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show $ groupId group)))]
  resourceMetaData        = const metaEmpty
  resourceRelationships   = const $ Relationships HM.empty

-- These resource types are used to associate one type with its relationships 
-- The ToJSON and FromJSON should ignore the relationships
-- ResourceEntity toResource should also set the resources to empty list or nothing

data UserResource =
  UserResource 
    { urUser      :: User
    , urFriends   :: [User]
    , urBoss      :: Maybe User
    , urBlogPosts :: [BlogPost]
    } deriving (Eq, Generic, Read, Show)

instance ToJSON UserResource where
  toJSON (UserResource u _friends _boss _blogposts) = toJSON u

instance FromJSON UserResource where
  parseJSON o =
    UserResource <$> parseJSON o <*> pure [] <*> pure Nothing <*> pure []
    
instance ResourceEntity UserResource where
  resourceIdentifier       = T.pack . show . userId . urUser
  resourceType             = const "users"
  resourceLinks         ur = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show . userId . urUser $ ur)))]
  resourceMetaData         = const metaEmpty
  resourceRelationships ur = Relationships . HM.fromList $ (catMaybes $ friends ++ boss ++ blogposts)
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) metaEmpty
      friends = [mkKeyRelationshipPair "friends" (mkIdentifier <$> urFriends ur) linksEmpty]
      boss    = 
        case urBoss ur of
          Nothing  -> []
          Just bss -> [mkKeyRelationshipPair "boss" [mkIdentifier bss] linksEmpty]
      blogposts = [mkKeyRelationshipPair "blogposts" (mkIdentifier <$> urBlogPosts ur) linksEmpty]
      
  toResource ur =
    Resource
      (Identifier (resourceIdentifier ur) (resourceType ur) (resourceMetaData ur))
      (ur { urFriends = [], urBoss = Nothing, urBlogPosts = [] } )
      (resourceLinks ur)
      (resourceRelationships ur)


instance DocumentEntity UserResource where
  toDocument urs = Document (toResource <$> urs) linksEmpty metaEmpty (friends <> boss <> blogposts)
      where
        friends   = includedFromResources urs urFriends
        boss      = includedFromResources2 urs urBoss
        blogposts = includedFromResources urs urBlogPosts
  
  fromDocument doc = updateResource <$> docData doc
    where
      updateResource r = userR { urFriends   = resourcesFromIncluded (identifiersFromResourceRelationships "friends" r) (docIncluded doc)
                               , urBoss      = listToMaybe $ resourcesFromIncluded (identifiersFromResourceRelationships "boss" r) (docIncluded doc)
                               , urBlogPosts = resourcesFromIncluded (identifiersFromResourceRelationships "blogposts" r) (docIncluded doc)
                               }
        where
          userR = fromResource r

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

instance ResourceEntity GroupResource where
  resourceIdentifier       = T.pack . show . groupId . grGroup
  resourceType             = const "groups"
  resourceLinks         gr = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show . groupId . grGroup $ gr)))]
  resourceMetaData         = const metaEmpty
  resourceRelationships gr = Relationships . HM.fromList . catMaybes $ [mkKeyRelationshipPair "members" (mkIdentifier <$> grUsers gr) linksEmpty]
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) metaEmpty
  toResource gr =
    Resource
      (Identifier (resourceIdentifier gr) (resourceType gr) (resourceMetaData gr))
      (gr { grUsers = [] } )
      (resourceLinks gr)
      (resourceRelationships gr)

instance DocumentEntity GroupResource where
  toDocument grs = Document (toResource <$> grs) linksEmpty metaEmpty (members)
      where
        members = includedFromResources grs grUsers
  
  fromDocument doc = updateResource <$> docData doc
    where
      updateResource r = groupR { grUsers = resourcesFromIncluded (identifiersFromResourceRelationships "members" r) (docIncluded doc) }
        where
          groupR = fromResource r
