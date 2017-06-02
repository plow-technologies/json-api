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
import qualified Data.HashSet as HS
import           Data.JSONAPI.Internal.Identifier (Identifier (..))
import           Data.JSONAPI.Internal.Link (Links(..), linksEmpty)
import           Data.JSONAPI.Internal.Util ((.=#), (.:@))
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- Relationship name does not come from the identifier

data Relationship =
  Relationship
    { rlIdentifiers :: HS.HashSet Identifier
    , rlLinks       :: Links
    } deriving (Eq, Generic, Read, Show)

instance Hashable Relationship

instance ToJSON Relationship where
  toJSON (Relationship rlIdntifiers (Links lnks)) =
    object (identifiers ++ "links" .=# lnks)
    where
      identifiers = 
        case HS.size rlIdntifiers of
          0 -> []
          1 -> ["data" .= (head $ HS.toList rlIdntifiers)]
          _ -> ["data" .= rlIdntifiers]
          
instance FromJSON Relationship where
  parseJSON = withObject "Relationship" $ \o -> do
    --mIdentifiers <- o .:? "data"
    mLnks <- o .:? "links"
    {-
    let identifiers = case mIdentifiers of
          Nothing    -> HS.empty
          Just idnts -> idnts
    -}
    let lnks = case mLnks of 
          Nothing    -> linksEmpty
          Just jlnks -> jlnks
    Relationship <$> (HS.fromList <$> o .:@ "data")
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
    _ -> Just $ Relationship HS.empty links
mkRelationship identifiers links = Just $ Relationship (HS.fromList identifiers) links

relationshipsEmpty :: Relationships
relationshipsEmpty = Relationships HM.empty
