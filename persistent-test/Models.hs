{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.JSONAPI hiding (Error)

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Data.Vector as V

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  deriving Eq Read Show

Group
  name Text
  deriving Eq Read Show
  
GroupUserJoin
  userId  UserId
  groupId GroupId
  deriving Eq Read Show

BlogPost
  contents Text
  userId   UserId
  deriving Eq Read Show
|]

instance ToJSON User where
  toJSON (User uname) =
    object 
      [ "name"    .= uname
      ]
      
instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "name"

instance ToJSON Group where
  toJSON (Group gname) = 
    object 
      [ "name" .= gname ]
      
instance FromJSON Group where
  parseJSON = withObject "Group" $ \o ->
    Group <$> o .: "name"

instance ToJSON BlogPost where
  toJSON (BlogPost _contents _userId) =
    object
      [ "contents" .= _contents
      , "userId"  .= _userId
      ]

instance FromJSON BlogPost where
  parseJSON = withObject "BlogPost" $ \o ->
    BlogPost <$> o .: "contents"
             <*> o .: "userId"

-- Resource types are to help martial Relationships into and out of Documents

data UserResource = 
  UserResource {
    urUser     :: (Entity User)
  , urGroups   :: [Entity Group]
  , urBlogPosts :: [Entity BlogPost]
  } deriving (Eq, Read, Show)

instance ToJSON UserResource where
  toJSON (UserResource user _groups _commnets) = toJSON user
  
instance FromJSON UserResource where
  parseJSON o =
    UserResource <$> parseJSON o <*> pure [] <*> pure []
  
data GroupResource =
  GroupResource {
    grGroup  :: (Entity Group)
  , grUsers  :: [Entity User] 
  } deriving (Eq, Read, Show)

instance ToJSON GroupResource where
  toJSON (GroupResource g _us) = toJSON g
  
instance FromJSON GroupResource where
  parseJSON o =
    GroupResource <$> parseJSON o <*> pure []
  
instance (ToJSON a) => ToJSON (Entity a) where
  toJSON (Entity key value) =
    object 
      [ "key"   .= key
      , "value" .= value
      ]

instance (PersistEntity a, FromJSON a) => FromJSON (Entity a) where
  parseJSON = withObject "Entity" $ \o ->
    Entity <$> o .: "key"
           <*> o .: "value"

instance ResourceEntity (Entity User) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "users"
  resourceLinks      user = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show $ entityKey user)))]
  resourceMetaData        = const metaEmpty
  resourceRelationships   = const $ Relationships HM.empty

instance ResourceEntity (Entity Group) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "groups"
  resourceLinks     group = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show $ entityKey group)))]
  resourceMetaData        = const metaEmpty
  resourceRelationships   = const $ Relationships HM.empty
  
instance ResourceEntity (Entity BlogPost) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "blogposts"
  resourceLinks  blogpost = mkLinks [("self", LinkHref ("/api/blog/posts/" <> (T.pack . show $ entityKey blogpost)))]
  resourceMetaData        = const metaEmpty
  resourceRelationships   = const $ Relationships HM.empty

instance ResourceEntity UserResource where
  resourceIdentifier       = T.pack . show . entityKey . urUser
  resourceType             = const "users"
  resourceLinks         ur = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show . entityKey . urUser $ ur)))]
  resourceMetaData         = const metaEmpty
  resourceRelationships ur = Relationships . HM.fromList $ (catMaybes $ groups ++ blogposts)
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) metaEmpty
      groups    = [mkKeyRelationshipPair "groups"    (mkIdentifier <$> urGroups ur)    linksEmpty]
      blogposts = [mkKeyRelationshipPair "blogposts" (mkIdentifier <$> urBlogPosts ur) linksEmpty]
      
  toResource ur =
    Resource
      (Identifier (resourceIdentifier ur) (resourceType ur) (resourceMetaData ur))
      (ur { urGroups = [], urBlogPosts = [] } )
      (resourceLinks ur)
      (resourceRelationships ur)
        
instance ResourceEntity GroupResource where
  resourceIdentifier       = T.pack . show . entityKey . grGroup
  resourceType             = const "groups"
  resourceLinks         gr = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show . entityKey . grGroup $ gr)))]
  resourceMetaData         = const metaEmpty
  resourceRelationships gr = Relationships . HM.fromList $ catMaybes [mkKeyRelationshipPair "members" (mkIdentifier <$> grUsers gr) linksEmpty]
    where
      mkIdentifier user =
        Identifier (resourceIdentifier user) (resourceType user) metaEmpty
  toResource gr =
    Resource
      (Identifier (resourceIdentifier gr) (resourceType gr) (resourceMetaData gr))
      (gr { grUsers = [] } )
      (resourceLinks gr)
      (resourceRelationships gr)
      
instance DocumentEntity UserResource where
  toDocument urs = Document (toResource <$> urs) linksEmpty metaEmpty (groups <> blogposts)
      where
        groups    = includedFromResources urs urGroups
        blogposts = includedFromResources urs urBlogPosts
  
  fromDocument doc = updateResource <$> docData doc
    where
      updateResource r = userR { urGroups   = resourcesFromIncluded (identifiersFromResourceRelationships  "groups" r) (docIncluded doc)
                               , urBlogPosts = resourcesFromIncluded (identifiersFromResourceRelationships "blogposts" r) (docIncluded doc)
                               }
        where
          userR = fromResource r

