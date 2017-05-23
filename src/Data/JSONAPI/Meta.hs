{-# LANGUAGE DeriveGeneric #-}

{- |
Module representing a JSON-API meta object.

Specification: <http://jsonapi.org/format/#document-meta>

Where specified, a meta member can be used to include non-standard meta-information. The value of each meta member MUST be an object (a “meta object”).

Any members MAY be specified within meta objects.
-}

module Data.JSONAPI.Meta (
    Meta (..)
  , MetaObject (..)
  , mkMeta
  ) where

import           Data.Aeson ( Object
                            , FromJSON, parseJSON, withObject
                            , ToJSON, toJSON, object)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           GHC.Generics (Generic)

newtype Meta = Meta Object deriving (Eq, Generic, Read, Show)

instance ToJSON Meta where
  toJSON (Meta o) = object $ HM.toList o

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \o -> return $ Meta o
  
instance Monoid Meta where
  mappend (Meta a) (Meta b) = Meta $ HM.union a b
  mempty = Meta $ HM.empty

class (ToJSON a) => MetaObject a where
  typeName :: a -> Text

mkMeta :: (MetaObject a) => a -> Meta
mkMeta obj = Meta $ HM.singleton (typeName obj) (toJSON obj)