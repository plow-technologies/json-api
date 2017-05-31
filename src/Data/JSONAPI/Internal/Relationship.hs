{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONAPI.Internal.Relationship (
   Relationship  (..)
 , Relationships (..)
 , relationshipsEmpty
 ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Identifier (Identifier (..))
import           Data.JSONAPI.Internal.Link (Links(..), linksEmpty)
import           Data.JSONAPI.Internal.Util ((.=@),(.:@),(.=#))
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- Relationship name does not come from the identifier

data Relationship =
  Relationship
    { rlIdentifiers :: [Identifier] -- can be multiple
    , rlLinks       :: Links
    } deriving (Eq, Generic, Read, Show)

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
  
instance ToJSON Relationships where
  toJSON (Relationships o) = object $ (\(x,y) -> (x,toJSON y)) <$> HM.toList o

instance FromJSON Relationships where
  parseJSON = withObject "Relationships" $ \o -> 
    Relationships <$> (sequence $ parseJSON <$> o)
    
instance Monoid Relationships where
  mappend (Relationships a) (Relationships b) = Relationships $ HM.union a b
  mempty = Relationships $ HM.empty

relationshipsEmpty :: Relationships
relationshipsEmpty = Relationships HM.empty
