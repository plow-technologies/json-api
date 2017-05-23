{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Document where

import           Data.Aeson
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Util ((.=?), (.=@))
import qualified Data.JSONAPI.Error as E
import           Data.JSONAPI.Link (Links)
import           Data.JSONAPI.Meta (Meta)
import           Data.JSONAPI.Resource

data (ResourcefulEntity a) => Document a =
  Document 
    { _data     :: [Resource a] -- should never be empty
    , _links    :: Maybe Links
    , _meta     :: Maybe Meta
    , _included :: [Value] -- can be empty
    } deriving (Eq, Read, Show)

instance (ResourcefulEntity a, ToJSON a) => ToJSON (Document a) where
  toJSON (Document {..}) =
    object 
      (     "data"     .=@ _data
        ++  "included" .=@ _included
        ++  "links"    .=? _links
        ++  "meta"     .=? _meta
      )

instance (ResourcefulEntity a, FromJSON a) => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> do 
    let documentData = case HM.lookup "data" o of
          Just a@(Array _a)  -> parseJSON a
          Just o@(Object _o) -> (:[]) <$> parseJSON o
          _                  -> pure []
      
        included = case HM.lookup "included" o of
          Just (Array a)     -> F.toList a
          Just o@(Object _o) -> [o] -- singleton
          _                  -> []
    
    Document 
      <$> documentData
      <*> o .:? "links"
      <*> o .:? "meta"
      <*> pure included
             
data ErrorDocument =
  ErrorDocument
    { _error :: E.Error
    , _errorLinks :: Maybe Links
    , _errorMeta  :: Maybe Meta
    } deriving (Eq, Read, Show)

instance ToJSON (ErrorDocument) where
  toJSON (ErrorDocument {..}) =
    object
      ([ "error" .= _error ] ++ ("links" .=? _errorLinks) ++ ("meta" .=? _errorMeta))

instance FromJSON (ErrorDocument) where
  parseJSON = withObject "Error" $ \o ->
    ErrorDocument <$> o .:  "error"
                  <*> o .:? "links"
                  <*> o .:? "meta"
