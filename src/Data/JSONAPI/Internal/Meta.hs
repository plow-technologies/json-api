{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module representing a JSON-API meta object.

Specification: <http://jsonapi.org/format/#document-meta>

Where specified, a meta member can be used to include non-standard meta-information. The value of each meta member MUST be an object (a “meta object”).

Any members MAY be specified within meta objects.
-}

module Data.JSONAPI.Internal.Meta (
    Meta (..)
  , MetaObject (..)
  , metaEmpty
  , mkMeta
  ) where

import           Data.Aeson ( Object
                            , FromJSON, parseJSON, withObject
                            , ToJSON, toJSON, object)
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           GHC.Generics (Generic)

newtype Meta = Meta Object deriving (Eq, Generic, Read, Show)

instance Hashable Meta

instance Monoid Meta where
  mappend (Meta a) (Meta b) = Meta $ HM.union a b
  mempty = Meta $ HM.empty

instance ToJSON Meta where
  toJSON (Meta o) = object $ HM.toList o

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \o -> return $ Meta o


class (ToJSON a) => MetaObject a where
  typeName :: a -> Text

mkMeta :: (MetaObject a) => a -> Meta
mkMeta obj = Meta $ HM.singleton (typeName obj) (toJSON obj)

metaEmpty :: Meta
metaEmpty = Meta HM.empty