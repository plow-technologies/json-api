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
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Meta (Meta(..), metaEmpty)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data Identifier =
  Identifier
    { idId   :: Text
    , idType :: Text
    , idMeta :: Meta -- if size is 0 then do not add "meta" key to JSON
    } deriving (Eq, Generic, Read, Show)

instance Hashable Identifier

instance ToJSON Identifier where
  toJSON (Identifier _idId _idType _idMeta@(Meta o)) =
    object (["id" .= _idId, "type" .= _idType] ++ meta)
    where
      meta = 
        case HM.size o of
          0 -> []
          _ -> ["meta" .= _idMeta]

instance FromJSON Identifier where
 parseJSON = withObject "Identifier" $ \o -> do
   mMeta <- o .:? "meta" 
   let meta = 
         case mMeta of
           Nothing  -> metaEmpty
           Just mta -> mta
   Identifier <$> o .:  "id"
              <*> o .:  "type"
              <*> pure meta

class HasIdentifier a where
  identifier :: a -> Identifier