{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Internal.Document (
    Document (..)
  , DocumentEntity (..)
  , ErrorDocument (..)
  , Included (..)
  , includedEmpty
  , mkIncluded
  , parseIncludedResources
  , resourcesFromIncluded
  ) where

import           Data.Aeson
import qualified Data.JSONAPI.Internal.Error as E
import           Data.JSONAPI.Internal.Identifier (Identifier)
import           Data.JSONAPI.Internal.Link (Links)
import           Data.JSONAPI.Internal.Meta (Meta)
import           Data.JSONAPI.Internal.Resource
import           Data.JSONAPI.Internal.Util ((.=?), (.=@), (.:@))
import           Data.Maybe (catMaybes)
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HM
-- import qualified Data.Foldable as F

newtype Included = Included Array deriving (Eq, Generic, Read, Show)

instance ToJSON Included where
  toJSON (Included arr) = toJSON arr

instance FromJSON Included where
  parseJSON = withArray "Included" $ \arr -> return $ Included arr

includedEmpty :: Included
includedEmpty = Included $ V.empty

mkIncluded :: ToJSON a => [a] -> Included
mkIncluded as = Included . V.fromList $ toJSON <$> as

parseIncludedResources :: (ResourceEntity a) => Included -> [Resource a]
parseIncludedResources (Included arr) = catMaybes . V.toList $ resultToMaybe . fromJSON <$> arr
  where
    resultToMaybe r =
      case r of
        Error   _ -> Nothing
        Success a -> Just a

-- | Included is a heterogenous array of types (JSON objects) stored in an
-- Array (Vector) of Objects (HashMap). Use this function to retrieve resources 
-- and build a sum type that encodes top level Document type and resource 
-- types.
resourcesFromIncluded :: (ResourceEntity a) => [Identifier] -> Included -> [a]
resourcesFromIncluded identifiers included = fromResource <$> filter (\identifier -> (rsIdentifier identifier) `elem` identifiers) (parseIncludedResources included)

data (ResourceEntity a) => Document a =
  Document 
    { docData      :: [Resource a] -- should never be empty
    , docLinks     :: Maybe Links
    , docMeta      :: Maybe Meta
    , docIncluded  :: Included -- [Value] -- if exists should be (Array [Object, Object,...])
    } deriving (Eq, Read, Show)

instance (ResourceEntity a, ToJSON a) => ToJSON (Document a) where
  toJSON (Document _docData _docLinks _docMeta _docIncluded) =
    object 
      (     "data"     .=@  _docData
        ++  "links"    .=?  _docLinks
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
    let included = case HM.lookup "included" o of
          Just (Array arr) -> Included arr
          _                   -> includedEmpty

    Document 
      <$> o .:@ "data"
      <*> o .:? "links"
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