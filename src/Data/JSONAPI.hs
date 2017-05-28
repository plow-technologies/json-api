module Data.JSONAPI (
 -- Types
 -- on the right side is a list of each type's internal dependency
   Document(..)      -- Resource, Relationship, Link, Identifier, Meta
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
 , mkMeta
 
 -- Values
 , emptyLinks
 , emptyRelationships
 
 -- Type Classes
 , HasIdentifier  (..)
 , MetaObject     (..)
 , ResourceEntity (..)
 , DocumentEntity (..)
 ) where

import Data.JSONAPI.Internal.Document
import Data.JSONAPI.Internal.Error
import Data.JSONAPI.Internal.Identifier
import Data.JSONAPI.Internal.Link
import Data.JSONAPI.Internal.Meta
import Data.JSONAPI.Internal.Relationship
import Data.JSONAPI.Internal.Resource