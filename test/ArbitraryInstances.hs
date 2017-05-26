{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.JSONAPI.Document
import           Data.JSONAPI.Identifier
import           Data.JSONAPI.Link
import           Data.JSONAPI.Meta
import           Data.JSONAPI.Relationship
import           Data.JSONAPI.Resource
import qualified Data.Text as T
import           Data.Text (Text)
import           Test.QuickCheck
  
instance Arbitrary Meta where
  arbitrary = Meta <$> (HM.fromList <$> arbitrary)

instance Arbitrary Identifier where
  arbitrary = Identifier <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary LinkObject where
  arbitrary = LinkObject <$> arbitrary <*> arbitrary
  
instance Arbitrary Link where
  arbitrary = oneof [LinkHref <$> arbitrary, LinkLinkObject <$> arbitrary]
    

instance Arbitrary Links where
  arbitrary = do 
    i     <- choose (0,10)
    lnks  <- vector i :: Gen [(Text,Link)]
    return $ Links (HM.fromList lnks)

instance Arbitrary Relationship where
  arbitrary = Relationship <$> arbitrary <*> arbitrary

instance Arbitrary Relationships where
  arbitrary =  do
    i             <- choose (0,10)
    rlationships  <- vector i :: Gen [(Text,Relationship)]
    return $ Relationships (HM.fromList rlationships)
        
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Value where
  arbitrary = do 
    i  <- choose (0,10)
    keys   <- vector i :: Gen [Text]
    values <- fmap String <$> vector i :: Gen [Value]
    return $ Object $ HM.fromList $ zip keys values
  
instance Arbitrary a => Arbitrary (Resource a) where
  arbitrary = Resource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (ResourcefulEntity a, Arbitrary a) => Arbitrary (Document a) where
  arbitrary = do
    resourceSize <- choose (1,5)
    includeSize  <- choose (0,5)
    Document <$> vector resourceSize <*> arbitrary <*> arbitrary <*> vector includeSize
    