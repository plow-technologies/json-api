{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.JSONAPI.Identifier (
   HasIdentifier (..)
 , Identifier (..)
 ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Meta (Meta)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Identifier =
  Identifier 
    { ident    :: Text
    , datatype :: Text
    , metadata :: Maybe Meta
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Identifier where
  toJSON (Identifier {..}) =
    object (["id" .= ident, "type" .= datatype] ++ metadataPair)
    where
      metadataPair = case metadata of
        Nothing -> []
        Just m  -> ["meta" .= m]

instance FromJSON Identifier where
 parseJSON = withObject "Identifier" $ \o ->
   Identifier <$> o .:  "id"
              <*> o .:  "type"
              <*> o .:? "meta"
              
class HasIdentifier a where
  identifier :: a -> Identifier