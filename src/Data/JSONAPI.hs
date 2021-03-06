{-# LANGUAGE CPP #-}

module Data.JSONAPI (
 -- Types
 -- on the right side is a list of each type's internal dependency
   Document(..)      -- Resource, Relationship, Link, Identifier, Meta, Included
 , Included (..)
 , ErrorDocument(..) -- Error, Link, Meta
 , Error(..)         -- Link, Meta
 , Resource(..)      -- Identifier, Link, Meta, Relationship
 , Relationship(..)  -- Link, Identifier 
 , Relationships(..) -- Relationship, Link, Identifier
 , Link(..)          -- Meta
 , LinkObject(..)
 , Links(..)
 , Identifier(..)    -- Meta
 , Meta(..)
 
 -- Constructors
 , mkLinks
 , mkIncluded
 , mkMeta
 , mkKeyRelationshipPair
 , mkRelationship
 , mkSimpleLinks
 
 -- Helper Functions
 , identifiersFromResourceRelationships
 , includedFromResources
 , includedFromResources2
 , parseIncludedResources
 , resourcesFromIncluded
 , toDocumentComplete
 , fromDocumentComplete
 
 -- Values
 , includedEmpty
 , linksEmpty
 , metaEmpty
 , relationshipsEmpty
 
 -- Type Classes
 , HasIdentifier  (..)
 , MetaObject     (..)
 , ResourceEntity (..)
 , DocumentEntity (..)
 ) where

import Data.JSONAPI.Internal.Document
import Data.JSONAPI.Internal.Error
import Data.JSONAPI.Internal.Identifier
import Data.JSONAPI.Internal.Included
import Data.JSONAPI.Internal.Link
import Data.JSONAPI.Internal.Meta
import Data.JSONAPI.Internal.Relationship
import Data.JSONAPI.Internal.Resource

#if defined(ghcjs_HOST_OS)
import Data.JSONAPI.Internal.GHCJS.Instances ()  
#endif