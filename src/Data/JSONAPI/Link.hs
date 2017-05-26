{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module representing a JSON-API link object.

Specification: <http://jsonapi.org/format/#document-links>
-}

module Data.JSONAPI.Link (
    Link  (..)
  , LinkObject (..)
  , Links (..)
  , emptyLinks
  , mkLinks
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Internal.Util ((.=?))
import           Data.JSONAPI.Meta (Meta)
import           Data.Text (Text)
import           GHC.Generics (Generic)

{- |
Each member of a links object is a “link”. A link MUST be represented as either:

 - a string containing the link’s URL.
 - an object (“link object”) which can contain the following members:
   - href: a string containing the link’s URL.
   - meta: a meta object containing non-standard meta-information about the link.

Type representing a JSON-API link object.

Links are an abstraction around an underlying Map consisting of
relevance identifiers as keys and URIs as values.

Example JSON:
@
  "links": {
    "self": "http://example.com/posts/1"
  }
@

@
  "links": {
    "related": {
      "href": "http://example.com/articles/1/comments",
      "meta": {
        "count": 10
      }
    }
  }
@

Specification: <http://jsonapi.org/format/#document-links>
-}

mkLinks :: [(Text,Link)] -> Links
mkLinks = Links . HM.fromList

emptyLinks :: Links 
emptyLinks = Links HM.empty

newtype Links = Links (HM.HashMap Text Link) 
  deriving (Eq, Generic, Read, Show)

instance ToJSON Links where
  toJSON (Links o) = object $ (\(x,y) -> (x,toJSON y)) <$> HM.toList o

instance FromJSON Links where
  parseJSON = withObject "Links" $ \o -> 
    Links <$> (sequence $ parseJSON <$> o)
    
instance Monoid Links where
  mappend (Links a) (Links b) = Links $ HM.union a b
  mempty = Links $ HM.empty

-- type Link

data Link 
  = LinkHref Text -- href only
  | LinkLinkObject LinkObject
  deriving (Eq, Generic, Read, Show)

instance ToJSON Link where
  toJSON (LinkHref _href) = String _href
  toJSON (LinkLinkObject _linkObject) = toJSON _linkObject

instance FromJSON Link where
  parseJSON (String s) = return $ LinkHref s
  parseJSON v = LinkLinkObject <$> parseJSON v

data LinkObject = 
  LinkObject
    { href :: Text -- href
    , meta :: Maybe Meta
    } deriving (Eq, Generic, Read, Show)

instance ToJSON LinkObject where
  toJSON (LinkObject _href _meta) =
    object
      (["href" .= _href] ++ "meta" .=? _meta)

instance FromJSON LinkObject where
  parseJSON = withObject "LinkObject" $ \o ->
    LinkObject <$> o .:  "href"
               <*> o .:? "meta"