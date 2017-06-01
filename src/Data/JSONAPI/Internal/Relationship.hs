{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONAPI.Internal.Relationship (
   Relationship  (..)
 , Relationships (..)
 , mkRelationship
 , mkKeyRelationshipPair
 , relationshipsEmpty
 ) where

import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Identifier (Identifier (..))
import           Data.JSONAPI.Internal.Link (Links(..), linksEmpty)
import           Data.JSONAPI.Internal.Util ((.=@),(.:@),(.=#))
import           Data.List (nub)
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- Relationship name does not come from the identifier

data Relationship =
  Relationship
    { rlIdentifiers :: [Identifier] -- can be multiple
    , rlLinks       :: Links
    } deriving (Eq, Generic, Read, Show)

instance Hashable Relationship

instance ToJSON Relationship where
  toJSON (Relationship rlIdntifiers (Links lnks)) =
    object ("data" .=@ rlIdntifiers ++ "links" .=# lnks)

instance FromJSON Relationship where
  parseJSON = withObject "Relationship" $ \o -> do
    mLnks <- o .:? "links"
    let lnks = case mLnks of 
          Nothing    -> linksEmpty
          Just jlnks -> jlnks
    Relationship <$> o .:@ "data"
                 <*> pure lnks

newtype Relationships = Relationships (HM.HashMap Text Relationship)
  deriving (Eq, Generic, Read, Show)
  
instance Hashable Relationships

instance ToJSON Relationships where
  toJSON (Relationships o) = object $ (\(x,y) -> (x,toJSON y)) <$> HM.toList o

instance FromJSON Relationships where
  parseJSON = withObject "Relationships" $ \o -> 
    Relationships <$> (sequence $ parseJSON <$> o)
    
instance Monoid Relationships where
  mappend (Relationships a) (Relationships b) = Relationships $ HM.union a b
  mempty = Relationships $ HM.empty

mkKeyRelationshipPair :: Text -> [Identifier] -> Links -> Maybe (Text, Relationship)
mkKeyRelationshipPair t identifiers links = (,) <$> pure t <*> mkRelationship identifiers links

mkRelationship :: [Identifier] -> Links -> Maybe Relationship
mkRelationship [] links@(Links ls) =
  case HM.size ls of
    0 -> Nothing
    _ -> Just $ Relationship [] links
mkRelationship identifiers links = Just $ Relationship (nub identifiers) links

relationshipsEmpty :: Relationships
relationshipsEmpty = Relationships HM.empty
