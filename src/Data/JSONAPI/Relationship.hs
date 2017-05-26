{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.JSONAPI.Relationship (
   Relationship  (..)
 , Relationships (..)
 , emptyRelationships
-- , mkRelationship
-- , mkRelationships
 ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Util ((.=@),(.:@),(.=#))
import           Data.JSONAPI.Identifier (Identifier (..))
import           Data.JSONAPI.Link (Links(..), emptyLinks)
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- Relationship name does not come from the identifier

data Relationship =
  Relationship
    { identifiers :: [Identifier] -- can be multiple
    , links       :: Links
    } deriving (Eq, Generic, Read, Show)

instance ToJSON Relationship where
  toJSON (Relationship idntifiers (Links lnks)) =
    object ("data" .=@ idntifiers ++ "links" .=# lnks)

instance FromJSON Relationship where
  parseJSON = withObject "Relationship" $ \o -> do
    mLnks <- o .:? "links"
    let lnks = case mLnks of 
          Nothing    -> emptyLinks
          Just jlnks -> jlnks
    Relationship <$> o .:@ "data"
                 <*> pure lnks

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

{-
mkRelationship :: [Identifier] -> Maybe Links -> Maybe Relationship
mkRelationship [] Nothing = Nothing 
mkRelationship i l   = Just $ Relationship i l

mkRelationships :: Text -> Relationship -> Relationships
mkRelationships key rel = Relationships $ HM.singleton key rel
-}
emptyRelationships :: Relationships
emptyRelationships = Relationships HM.empty