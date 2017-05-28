{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.JSONAPI.Internal.Document (
    Document (..)
  , DocumentEntity (..)
  , ErrorDocument (..)
  ) where

import           Data.Aeson
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.JSONAPI.Internal.Error as E
import           Data.JSONAPI.Internal.Link (Links)
import           Data.JSONAPI.Internal.Meta (Meta)
import           Data.JSONAPI.Internal.Resource
import           Data.JSONAPI.Internal.Util ((.=?), (.=@), (.=@!), (.:@))
import           Data.Monoid ((<>))

import qualified Data.Vector as V

data (ResourceEntity a) => Document a =
  Document 
    { docData      :: [Resource a] -- should never be empty
    , docLinks     :: Maybe Links
    , docMeta      :: Maybe Meta
    , docIncluded  :: [Value] -- if exists should be (Array [Object, Object,...])
    } deriving (Eq, Read, Show)

instance (ResourceEntity a, ToJSON a) => ToJSON (Document a) where
  toJSON (Document _docData _docLinks _docMeta _docIncluded) =
    object 
      (     "data"     .=@  _docData
        ++  "links"    .=?  _docLinks
        ++  "meta"     .=?  _docMeta
        ++  "included" .=@! _docIncluded
      ) 
           
instance (ResourceEntity a, FromJSON a) => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> do     
    let included = case HM.lookup "included" o of
          Just a@(Array arr) -> [a]
          _                  -> []
              
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