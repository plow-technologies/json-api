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

import           Data.JSONAPI.Document
import           Data.JSONAPI.Identifier
import           Data.JSONAPI.Link
import           Data.JSONAPI.Meta
import           Data.JSONAPI.Relationship
import           Data.JSONAPI.Resource

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

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

instance ResourcefulEntity (Entity User) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "users"
  resourceLinks      user = mkLinks [("self", LinkHref ("/api/users/" <> (T.pack . show $ entityKey user)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty

instance ResourcefulEntity (Entity Group) where
  resourceIdentifier      = T.pack . show . entityKey
  resourceType            = const "groups"
  resourceLinks     group = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show $ entityKey group)))]
  resourceMetaData        = const Nothing
  resourceRelationships   = const $ Relationships HM.empty
  
instance ResourcefulEntity GroupResource where
  resourceIdentifier       = T.pack . show . entityKey . grGroup
  resourceType             = const "groups"
  resourceLinks         gr = mkLinks [("self", LinkHref ("/api/groups/" <> (T.pack . show . entityKey . grGroup $ gr)))]
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