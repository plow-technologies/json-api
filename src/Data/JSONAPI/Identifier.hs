{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{- |
Module representing the identifying part of a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}

module Data.JSONAPI.Identifier (
   HasIdentifier (..)
 , Identifier (..)
 ) where

import           Data.Aeson
import           Data.JSONAPI.Internal.Util ((.=?))
import           Data.JSONAPI.Meta (Meta)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Identifier =
  Identifier
    { ident    :: Text
    , datatype :: Text
    , metadata :: Maybe Meta -- if Nothing then do not add "meta" key to JSON
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Identifier where
  toJSON (Identifier {..}) =
    object (["id" .= ident, "type" .= datatype] ++ ("meta" .=? metadata))

instance FromJSON Identifier where
 parseJSON = withObject "Identifier" $ \o ->
   Identifier <$> o .:  "id"
              <*> o .:  "type"
              <*> o .:? "meta"

class HasIdentifier a where
  identifier :: a -> Identifier