{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Resource where

import Data.Aeson
-- import qualified Data.HashMap.Strict as HM
import Data.JSONAPI.Internal.Util ((.=?), (.=#))
import Data.JSONAPI.Identifier (Identifier(..), HasIdentifier(..))
import Data.JSONAPI.Link (Links(..), emptyLinks)
import Data.JSONAPI.Meta (Meta)
import Data.JSONAPI.Relationship (Relationships(..), emptyRelationships)
import Data.Text (Text)

data Resource a =
  Resource 
    { identifier    :: Identifier
    , resource      :: a
    , links         :: Links         -- (Links HM.HashMap) can be empty
    , relationships :: Relationships -- (Relationships HM.HashMap) can be empty
    } deriving (Eq, Read, Show)

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj (Links linksObj) (Relationships rels)) = 
    object (
      [ "id"            .= resId
      , "type"          .= resType
      , "attributes"    .= resObj
      ] ++ "meta" .=? metaObj ++ "links" .=# linksObj ++ "relationships" .=# rels)
      
instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = withObject "Resource" $ \o -> do
    idntifier <- Identifier <$> o .:  "id"
                            <*> o .:  "type"
                            <*> o .:? "meta"
    mLnks <- o .:? "links"
    mRsps <- o .:? "relationships"
    let lnks = case mLnks of
                 Nothing    -> emptyLinks
                 Just jlnks -> jlnks
        rsps = case mRsps of
                 Nothing    -> emptyRelationships
                 Just jrsps -> jrsps
    Resource <$> pure idntifier
             <*> o .:  "attributes"
             <*> pure lnks
             <*> pure rsps

instance HasIdentifier (Resource a) where
  identifier = Data.JSONAPI.Resource.identifier

class (ToJSON a, FromJSON a) => ResourcefulEntity a where
  resourceIdentifier    :: a -> Text
  resourceType          :: a -> Text
  resourceLinks         :: a -> Links
  resourceMetaData      :: a -> Maybe Meta
  resourceRelationships :: a -> Relationships

  fromResource :: Resource a -> a
  fromResource = resource

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)
