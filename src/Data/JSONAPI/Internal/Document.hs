{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Internal.Document (
    Document (..)
  , DocumentEntity (..)
  , ErrorDocument (..)
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
import           Data.JSONAPI.Internal.Meta (Meta)
import           Data.JSONAPI.Internal.Resource
import           Data.JSONAPI.Internal.Util ((.=?), (.=@), (.:@), (.=#))
import           Data.Maybe (catMaybes)
import qualified Data.Vector as V

import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F



data (ResourceEntity a) => Document a =
  Document 
    { docData      :: [Resource a] -- should never be empty
    , docLinks     :: Links
    , docMeta      :: Maybe Meta
    , docIncluded  :: Included -- [Value] -- if exists should be (Array [Object, Object,...])
    } deriving (Eq, Read, Show)

instance (ResourceEntity a, ToJSON a) => ToJSON (Document a) where
  toJSON (Document _docData (Links _docLinks) _docMeta _docIncluded) =
    object 
      (     "data"     .=@  _docData
        ++  "links"    .=#  _docLinks
        ++  "meta"     .=?  _docMeta
        ++  (included _docIncluded)
      )
    where
      included (Included arr) =
        case V.length arr of
          0 -> []
          _ -> ["included" .= _docIncluded]
           
instance (ResourceEntity a, FromJSON a) => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> do
    mLinks <- o .:? "links"
    let included = case HM.lookup "included" o of
          Just (Array arr) -> Included arr
          _                -> includedEmpty
        links = case mLinks of 
              Nothing    -> linksEmpty
              Just jlnks -> jlnks
          
    Document 
      <$> o .:@ "data"
      <*> pure links
      <*> o .:? "meta"
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

class (ResourceEntity a) => DocumentEntity a where
  fromDocument :: Document a -> [a]
  toDocument   :: [a] -> Document a
  
-- | Included is a heterogenous array of types (JSON objects) stored in an
-- Array (Vector) of Objects (HashMap). Use this function to retrieve resources 
-- and build a sum type that encodes top level Document type and resource 
-- types.
resourcesFromIncluded :: (ResourceEntity a) => [Identifier] -> Included -> [a]
resourcesFromIncluded identifiers included = fromResource <$> filter (\identifier -> (rsIdentifier identifier) `elem` identifiers) (parseIncludedResources included)

includedFromResources :: (ResourceEntity a, ResourceEntity b) => [a] -> (a -> [b]) -> Included
includedFromResources xs f = mkIncluded (F.concat $ fmap toResource <$> f <$> xs)

includedFromResources2 :: (ResourceEntity a, ResourceEntity b) => [a] -> (a -> Maybe b) -> Included
includedFromResources2 xs f = mkIncluded (catMaybes $ fmap toResource <$> f <$> xs)

parseIncludedResources :: (ResourceEntity a) => Included -> [Resource a]
parseIncludedResources (Included arr) = catMaybes . V.toList $ resultToMaybe . fromJSON <$> arr
  where
    resultToMaybe r =
      case r of
        Error   _ -> Nothing
        Success a -> Just a
