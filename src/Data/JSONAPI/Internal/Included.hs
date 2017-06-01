{-# LANGUAGE DeriveGeneric #-}
module Data.JSONAPI.Internal.Included (
   Included (..)
 , includedEmpty
 , mkIncluded
 ) where

import Data.Aeson
import Data.Hashable
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Data.Vector.Instances ()
import GHC.Generics (Generic)

newtype Included = Included Array deriving (Eq, Generic, Read, Show)

instance Hashable Included

instance ToJSON Included where
  toJSON (Included arr) = toJSON arr

instance FromJSON Included where
  parseJSON = withArray "Included" $ \arr -> return $ Included arr

instance Monoid Included where
  mappend (Included a) (Included b) = Included $ a <> b
  mempty = Included $ V.empty

includedEmpty :: Included
includedEmpty = Included $ V.empty

mkIncluded :: ToJSON a => [a] -> Included
mkIncluded as = Included . V.fromList $ toJSON <$> as
