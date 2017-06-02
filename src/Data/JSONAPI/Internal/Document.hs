{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Internal.Document (
    Document (..)
  , DocumentEntity (..)
  , ErrorDocument (..)
  
  , fromDocumentComplete
  , toDocumentComplete
  
  , includedFromResources
  , includedFromResources2
  , parseIncludedResources
  , resourcesFromIncluded
  ) where

import           Data.Aeson
import qualified Data.JSONAPI.Internal.Error as E
import           Data.JSONAPI.Internal.Identifier (Identifier)
import           Data.JSONAPI.Internal.Included
import           Data.JSONAPI.Internal.Link (Links(..), linksEmpty)
import           Data.JSONAPI.Internal.Meta (Meta(..), metaEmpty)
import           Data.JSONAPI.Internal.Resource
import           Data.JSONAPI.Internal.Util ((.=?), (.=@), (.:@), (.=#))
import           Data.Maybe (catMaybes)
import qualified Data.Vector as V

import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F



data (Eq a, ResourceEntity a) => Document a =
  Document 
    { docData      :: [Resource a] -- should never be empty
    , docLinks     :: Links
    , docMeta      :: Meta
    , docIncluded  :: Included -- [Value] -- if exists should be (Array [Object, Object,...])
    } deriving (Eq, Read, Show)

instance (Eq a, ResourceEntity a, ToJSON a) => ToJSON (Document a) where
  toJSON (Document _docData (Links _docLinks) _docMeta@(Meta o) _docIncluded@(Included arr)) =
    object 
      (     "data"     .=@  _docData
        ++  "links"    .=#  _docLinks
        ++  meta
        ++  included
      )
    where
      included =
        case V.length arr of
          0 -> []
          _ -> ["included" .= _docIncluded]
      meta =
        case HM.size o of
          0 -> []
          _ -> ["meta" .= _docMeta]
           
instance (Eq a, ResourceEntity a, FromJSON a) => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> do
    mMeta  <- o .:? "meta"
    mLinks <- o .:? "links"
    let included = case HM.lookup "included" o of
          Just (Array arr) -> Included arr
          _                -> includedEmpty
        links = case mLinks of 
              Nothing    -> linksEmpty
              Just jlnks -> jlnks
        meta = case mMeta of
              Nothing  -> metaEmpty
              Just mta -> mta  
    Document 
      <$> o .:@ "data"
      <*> pure links
      <*> pure meta
      <*> pure included
             
data ErrorDocument =
  ErrorDocument
    { errDocError :: E.Error
    , errDocLinks :: Maybe Links
    , errDocMeta  :: Maybe Meta
    } deriving (Eq, Read, Show)

instance ToJSON (ErrorDocument) where
  toJSON (ErrorDocument _errDocError _errDocLinks _errDocMeta) =
    object
      (  ["error" .=  _errDocError ] 
      ++ ("links" .=? _errDocLinks) 
      ++ ("meta"  .=? _errDocMeta)
      )

instance FromJSON (ErrorDocument) where
  parseJSON = withObject "Error" $ \o ->
    ErrorDocument <$> o .:  "error"
                  <*> o .:? "links"
                  <*> o .:? "meta"

-- should move Relationships
class (ResourceEntity a) => DocumentEntity a where
  fromDocument :: Document a -> [a] -- construct [a] from docData and docIncluded
  toDocument   :: [a] -> Document a -- construct docIncluded with relationships from [a]


toDocumentComplete :: (DocumentEntity a, Eq a) => [a] -> Meta -> Links -> Document a
toDocumentComplete as meta links = doc { docMeta = meta, docLinks = links }
  where
    doc = toDocument as

fromDocumentComplete :: (DocumentEntity a, Eq a) => Document a -> ([a], Meta, Links)
fromDocumentComplete d = (fromDocument d, docMeta d, docLinks d)
  
-- | Included is a heterogenous array of types (JSON objects) stored in an
-- Array (Vector) of Objects (HashMap). Use this function to retrieve resources 
-- and build a sum type that encodes top level Document type and resource 
-- types.
resourcesFromIncluded :: (ResourceEntity a) => [Identifier] -> Included -> [a]
resourcesFromIncluded identifiers included = fromResource <$> filter (\identifier -> (rsIdentifier identifier) `elem` identifiers) (parseIncludedResources included)

includedFromResources :: (ResourceEntity b) => [a] -> (a -> [b]) -> Included
includedFromResources xs f = mkIncluded (F.concat $ fmap toResource <$> f <$> xs)

includedFromResources2 :: (ResourceEntity b) => [a] -> (a -> Maybe b) -> Included
includedFromResources2 xs f = mkIncluded (catMaybes $ fmap toResource <$> f <$> xs)

parseIncludedResources :: (ResourceEntity a) => Included -> [Resource a]
parseIncludedResources (Included arr) = catMaybes . V.toList $ resultToMaybe . fromJSON <$> arr
  where
    resultToMaybe r =
      case r of
        Error   _ -> Nothing
        Success a -> Just a
