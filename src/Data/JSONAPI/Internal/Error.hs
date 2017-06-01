{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONAPI.Internal.Error (
   Error (..)
 ) where

import Data.Aeson hiding (Error)
import Data.Text
import GHC.Generics (Generic)
import Data.Hashable
import Data.JSONAPI.Internal.Link (Links)
import Data.JSONAPI.Internal.Meta
import Data.JSONAPI.Internal.Util ((.=?))
import Prelude hiding (id)

data Error =
  Error
    { errId     :: Maybe Text
    , errLinks  :: Maybe Links
    , errStatus :: Maybe Text
    , errCode   :: Maybe Text
    , errDetail :: Maybe Text
    , errMeta   :: Maybe Meta
    } deriving (Eq, Generic, Read, Show)

instance Hashable Error

instance ToJSON Error where
  toJSON (Error id links status code detail meta) =
    object 
      (  "id"     .=? id
      ++ "links"  .=? links
      ++ "status" .=? status
      ++ "code"   .=? code
      ++ "detail" .=? detail
      ++ "meta"   .=? meta
      )

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o -> do
    Error <$> o .:? "id"
          <*> o .:? "links"
          <*> o .:? "status"
          <*> o .:? "code"
          <*> o .:? "detail"
          <*> o .:? "meta"
          
    