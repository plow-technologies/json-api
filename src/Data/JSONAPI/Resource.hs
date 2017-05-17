{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Resource where

import Data.Aeson
import Data.JSONAPI.Internal.Util ((.=?))
import Data.JSONAPI.Identifier (Identifier(..), HasIdentifier(..))
import Data.JSONAPI.Link (Links)
import Data.JSONAPI.Meta (Meta)
import Data.JSONAPI.Relationship (Relationships)
import Data.Text (Text)
import GHC.Generics (Generic)

data Resource a =
  Resource 
    { identifier    :: Identifier
    , resource      :: a
    , links         :: Maybe Links
    , relationships :: Maybe Relationships
    } deriving (Eq, Read, Show)

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj linksObj rels) = 
    object (
      [ "id"            .= resId
      , "type"          .= resType
      , "attributes"    .= resObj
      , "links"         .= linksObj
      , "meta"          .= metaObj
      , "relationships" .= rels
      ] ++ "meta" .=? metaObj ++ "link" .=? linksObj ++ "relationships" .=? rels)
      
instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = withObject "Resource" $ \o -> do
    identifier <- Identifier <$> o .:  "id"
                             <*> o .:  "type"
                             <*> o .:? "meta"
    Resource <$> pure identifier
             <*> o .:  "attributes"
             <*> o .:? "links"
             <*> o .:? "relationships"

instance HasIdentifier (Resource a) where
  identifier = Data.JSONAPI.Resource.identifier

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
