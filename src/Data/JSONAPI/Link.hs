{-# LANGUAGE DeriveGeneric #-}

module Data.JSONAPI.Link (
    Links (..)
  ) where

import Control.Arrow ((&&&))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics (Generic)

-- link name to url
newtype Links = Links (HM.HashMap Text Text) 
  deriving (Eq, Generic, Read, Show)

instance ToJSON Links where
  toJSON (Links o) = object $ (\(x,y) -> (x,toJSON y)) <$> HM.toList o

instance FromJSON Links where
  parseJSON = withObject "Links" $ \o -> 
    Links <$> (sequence $ parseJSON <$> o)
    
instance Monoid Links where
  mappend (Links a) (Links b) = Links $ HM.union a b
  mempty = Links $ HM.empty