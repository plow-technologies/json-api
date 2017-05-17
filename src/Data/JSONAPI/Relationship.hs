{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.JSONAPI.Relationship (
   Relationship  (..)
 , Relationships (..)
 , mkRelationship
 ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Util ((.=?))
import           Data.JSONAPI.Identifier (Identifier (..))
import           Data.JSONAPI.Link (Links)
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- Relationship name does not come from the identifier

data Relationship =
  Relationship
    { identifier :: Maybe Identifier
    , links      :: Maybe Links
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Relationship where
  toJSON (Relationship {..}) =
    object ("data" .=? identifier ++ "links" .=? links)

instance FromJSON Relationship where
  parseJSON = withObject "Relationship" $ \o ->
    Relationship <$> o .:? "data"
                 <*> o .:? "links"

newtype Relationships = Relationships (HM.HashMap Text Relationship)
  deriving (Eq, Generic, Read, Show)
  
instance ToJSON Relationships where
  toJSON (Relationships o) = object $ (\(x,y) -> (x,toJSON y)) <$> HM.toList o

instance FromJSON Relationships where
  parseJSON = withObject "Relationships" $ \o -> 
    Relationships <$> (sequence $ parseJSON <$> o)
    
instance Monoid Relationships where
  mappend (Relationships a) (Relationships b) = Relationships $ HM.union a b
  mempty = Relationships $ HM.empty

mkRelationship :: Maybe Identifier -> Maybe Links -> Maybe Relationship
mkRelationship Nothing Nothing = Nothing 
mkRelationship i l   = Just $ Relationship i l