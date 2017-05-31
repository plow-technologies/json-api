{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module representing the identifying part of a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}

module Data.JSONAPI.Internal.Identifier (
   HasIdentifier (..)
 , Identifier (..)
 ) where

import           Data.Aeson
import           Data.JSONAPI.Internal.Meta (Meta)
import           Data.JSONAPI.Internal.Util ((.=?))
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Identifier =
  Identifier
    { idId   :: Text
    , idType :: Text
    , idMeta :: Maybe Meta -- if Nothing then do not add "meta" key to JSON
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Identifier where
  toJSON (Identifier _idId _idType _idMeta) =
    object (["id" .= _idId, "type" .= _idType] ++ ("meta" .=? _idMeta))

instance FromJSON Identifier where
 parseJSON = withObject "Identifier" $ \o ->
   Identifier <$> o .:  "id"
              <*> o .:  "type"
              <*> o .:? "meta"

class HasIdentifier a where
  identifier :: a -> Identifier