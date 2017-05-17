{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.JSONAPI.Document where

import Data.Aeson
import Data.JSONAPI.Internal.Util ((.=?))
import qualified Data.JSONAPI.Error as E
import Data.JSONAPI.Link (Links)
import Data.JSONAPI.Meta (Meta)

data Document a =
  Document 
    { _data    :: [a]
    , _links   :: Maybe Links
    , _meta    :: Maybe Meta
    , _include :: [Value]
    } deriving (Eq, Read, Show)

instance (ToJSON a) => ToJSON (Document a) where
  toJSON (Document {..}) =
    object (
        [ "data" .= _data, "included" .= _include ]
        ++ ("links" .=? _links )
        ++ ("meta"  .=? _meta  )
      )

instance (FromJSON a) => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> 
    Document <$> o .:  "data"
             <*> o .:? "links"
             <*> o .:? "meta"
             <*> o .:  "included"
             
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
