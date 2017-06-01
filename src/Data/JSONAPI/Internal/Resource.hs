{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Internal.Resource (
   Resource (..)
 , ResourceEntity (..)
 , identifiersFromResourceRelationships
 ) where

import Data.Aeson
import Data.Hashable
import Data.JSONAPI.Internal.Identifier (Identifier(..), HasIdentifier(..))
import Data.JSONAPI.Internal.Link (Links(..), linksEmpty)
import Data.JSONAPI.Internal.Meta (Meta(..), metaEmpty)
import Data.JSONAPI.Internal.Relationship (Relationship(..), Relationships(..), relationshipsEmpty)
import Data.JSONAPI.Internal.Util ((.=#))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (Text)

data Resource a =
  Resource 
    { rsIdentifier    :: Identifier
    , rsResource      :: a
    , rsLinks         :: Links         -- (Links HM.HashMap) can be empty
    , rsRelationships :: Relationships -- (Relationships HM.HashMap) can be empty
    } deriving (Eq, Read, Show)

instance (Hashable a) => Hashable (Resource a) where
  hashWithSalt s (Resource i r l rs) = s `hashWithSalt` i `hashWithSalt` r `hashWithSalt` l `hashWithSalt` rs

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj@(Meta o)) resObj (Links linksObj) (Relationships rels)) = 
    object (
      [ "id"             .= resId
      , "type"           .= resType
      , "attributes"     .= resObj
      ] 
      ++ meta
      ++ "links"         .=# linksObj 
      ++ "relationships" .=# rels
    )
    where
      meta = 
        case HM.size o of
          0 -> []
          _ -> ["meta" .= metaObj]
      
instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = withObject "Resource" $ \o -> do
    mMeta <- o .:? "meta"
    let meta = case mMeta of
          Nothing    -> metaEmpty
          Just mta   -> mta
    idntifier <- Identifier <$> o .:  "id"
                            <*> o .:  "type"
                            <*> pure meta
    mLnks <- o .:? "links"
    mRsps <- o .:? "relationships"

    let lnks = case mLnks of
                 Nothing    -> linksEmpty
                 Just jlnks -> jlnks
        rsps = case mRsps of
                 Nothing    -> relationshipsEmpty
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
  resourceMetaData      :: a -> Meta
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
    Just rls -> HS.toList $ rlIdentifiers rls
  where
    unwrapRelationships (Relationships rls) = rls