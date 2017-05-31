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
  address Text
  deriving Eq Read Show

Group
  name Text
  deriving Eq Read Show
  
GroupUserJoin
  user  UserId
  group GroupId
  deriving Eq Read Show

Comment
  comment Text
  userId  UserId
  deriving Eq Read Show
|]

instance ToJSON User where
  toJSON (User uname uaddress) =
    object 
      [ "name"    .= uname
      , "address" .= uaddress
      ]
      
instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "name"
         <*> o .: "address"

instance ToJSON Group where
  toJSON (Group gname) = 
    object 
      [ "name" .= gname ]
      
instance FromJSON Group where
  parseJSON = withObject "Group" $ \o ->
    Group <$> o .: "name"

instance ToJSON Comment where
  toJSON (Comment _comment _userId) =
    object
      [ "comment" .= _comment
      , "userId"  .= _userId
      ]

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o ->
    Comment <$> o .: "comment"
            <*> o .: "userId"

data UserResource = 
  UserResource {
    urUser     :: (Entity User)
  , urGroups   :: [Entity Group]
  , urComments :: [Entity Comment]
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
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty

instance ResourceEntity (Entity Group) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "groups"
  resourceLinks     group = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show $ entityKey group)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty
  
instance ResourceEntity (Entity Comment) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "comments"
  resourceLinks   comment = mkLinks [("self", LinkHref ("/api/comments/" <> (T.pack . show $ entityKey comment)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty
  
instance ResourceEntity GroupResource where
  resourceIdentifier       = T.pack . show . entityKey . grGroup
  resourceType             = const "groups"
  resourceLinks         gr = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show . entityKey . grGroup $ gr)))]
  resourceMetaData         = const Nothing
  resourceRelationships gr = Relationships . HM.fromList $ [("members", (Relationship (mkIdentifier <$> grUsers gr) linksEmpty))]
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
instance DocumentEntity GroupResource where
  toDocument grs = Document (toResource <$> grs) Nothing Nothing (Just members)
      where
        members = mkIncluded (concat $ fmap toResource <$> grUsers <$> grs) 
  
  fromDocument doc = updateResource <$> docData doc
    where      
      getUsers :: [Identifier] -> Maybe Included -> [(Entity User)]
      getUsers is mvs = fromResource <$> filter (\u -> (rsIdentifier u) `elem` is) users
        where
          parseUsers :: Included -> [Resource (Entity User)] 
          parseUsers (Included arr) = catMaybes $ V.toList $ resultToMaybe . fromJSON <$> arr
          
          users = 
            case mvs of
              Nothing -> []
              Just vs -> parseUsers vs

      updateResource r = groupR { grUsers = getUsers (getRelationshipIdentifiers "members" (rsRelationships r)) (docIncluded doc) }
        where
          groupR = fromResource r 

getRelationshipIdentifiers :: Text -> Relationships -> [Identifier]
getRelationshipIdentifiers t (Relationships rs) =
  case HM.lookup t rs of
    Nothing -> []
    Just r  -> rlIdentifiers r

resultToMaybe :: Data.Aeson.Result a -> Maybe a
resultToMaybe x = 
  case x of
    Error _ -> Nothing
    Data.Aeson.Success a -> Just a
    
-}