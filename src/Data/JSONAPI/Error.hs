{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.JSONAPI.Error (
   Error (..)
 ) where

import Data.Aeson hiding (Error)
import Data.Text
import GHC.Generics (Generic)
import Data.JSONAPI.Link (Links)
import Data.JSONAPI.Meta
import Data.JSONAPI.Internal.Util ((.=?))
import Data.Text (Text)
import Prelude hiding (id)

data Error =
  Error
    { id     :: Maybe Text
    , links  :: Maybe Links
    , status :: Maybe Text
    , code   :: Maybe Text
    , detail :: Maybe Text
    , meta   :: Maybe Meta
    } deriving (Eq, Generic, Read, Show)
    
instance ToJSON Error where
  toJSON (Error {..}) =
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
          
    