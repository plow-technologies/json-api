{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Document where

import           Data.Aeson
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Util ((.=?), (.=@), (.:@))
import qualified Data.JSONAPI.Error as E
import           Data.JSONAPI.Link (Links)
import           Data.JSONAPI.Meta (Meta)
import           Data.JSONAPI.Resource
import           Data.Monoid ((<>))

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
    let included = case HM.lookup "included" o of
          Just (Array arr)     -> F.toList arr
          Just obj@(Object _o) -> [obj] -- singleton
          _                    -> []
    
    Document 
      <$> o .:@ "data"
      <*> o .:? "links"
      <*> o .:? "meta"
      <*> pure included

data Included = Included [Value]
  deriving (Eq, Read, Show)
  
instance Monoid Included where
  mempty = Included []
  mappend (Included as) (Included bs) = Included (as <> bs)
             
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
