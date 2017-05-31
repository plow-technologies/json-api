{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Internal.Resource (
   Resource (..)
 , ResourceEntity (..)
 , identifiersFromResourceRelationships
 ) where

import Data.Aeson
import Data.JSONAPI.Internal.Identifier (Identifier(..), HasIdentifier(..))
import Data.JSONAPI.Internal.Link (Links(..), emptyLinks)
import Data.JSONAPI.Internal.Meta (Meta)
import Data.JSONAPI.Internal.Relationship (Relationship(..), Relationships(..), emptyRelationships)
import Data.JSONAPI.Internal.Util ((.=?), (.=#))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

data Resource a =
  Resource 
    { rsIdentifier    :: Identifier
    , rsResource      :: a
    , rsLinks         :: Links         -- (Links HM.HashMap) can be empty
    , rsRelationships :: Relationships -- (Relationships HM.HashMap) can be empty
    } deriving (Eq, Read, Show)

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj (Links linksObj) (Relationships rels)) = 
    object (
      [ "id"             .= resId
      , "type"           .= resType
      , "attributes"     .= resObj
      ] 
      ++ "meta"          .=? metaObj 
      ++ "links"         .=# linksObj 
      ++ "relationships" .=# rels
    )
      
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
  identifier = rsIdentifier

class (ToJSON a, FromJSON a) => ResourceEntity a where
  resourceIdentifier    :: a -> Text
  resourceType          :: a -> Text
  resourceLinks         :: a -> Links
  resourceMetaData      :: a -> Maybe Meta
  resourceRelationships :: a -> Relationships

  fromResource :: Resource a -> a
  fromResource = rsResource

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)


-- | Useful function for extracting document relations.
identifiersFromResourceRelationships :: Text -> Resource a -> [Identifier]
identifiersFromResourceRelationships key rs =
  case HM.lookup key (unwrapRelationships $ rsRelationships rs) of
    Nothing -> []
    Just rls -> rlIdentifiers rls
  where
    unwrapRelationships (Relationships rls) = rls