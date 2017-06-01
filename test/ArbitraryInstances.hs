{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.JSONAPI
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V
import           Test.QuickCheck
  
instance Arbitrary Meta where
  arbitrary = do
    i     <- choose (1,3)
    Meta <$> (HM.fromList <$> vector i)

instance Arbitrary Included where
  arbitrary = do
    i     <- choose (0,3)
    Included <$> (V.fromList <$> vector i)

instance Arbitrary Identifier where
  arbitrary = Identifier <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LinkObject where
  arbitrary = LinkObject <$> arbitrary <*> arbitrary
  
instance Arbitrary Link where
  arbitrary = oneof [LinkHref <$> arbitrary, LinkLinkObject <$> arbitrary]

instance Arbitrary Links where
  arbitrary = do 
    i     <- choose (0,3)
    lnks  <- vector i :: Gen [(Text,Link)]
    return $ Links (HM.fromList lnks)

instance Arbitrary Relationship where
  arbitrary = do
    i             <- choose (0,3)
    idntifiers    <- HS.fromList <$> vector i
    Relationship <$> pure idntifiers <*> arbitrary

instance Arbitrary Relationships where
  arbitrary =  do
    i             <- choose (0,3)
    rlationships  <- vector i :: Gen [(Text,Relationship)]
    return $ Relationships (HM.fromList rlationships)
        
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Value where
  arbitrary = do 
    i      <- choose (0,3)
    keys   <- vector i :: Gen [Text]
    values <- fmap String <$> vector i :: Gen [Value]
    return $ Object $ HM.fromList $ zip keys values
  
instance Arbitrary a => Arbitrary (Resource a) where
  arbitrary = Resource <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Eq a, ResourceEntity a, Arbitrary a) => Arbitrary (Document a) where
  arbitrary = do
    resourceSize <- choose (1,3)
    Document <$> vector resourceSize <*> arbitrary <*> arbitrary <*> arbitrary