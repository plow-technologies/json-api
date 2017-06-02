{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.JSONAPI.Internal.GHCJS.Instances where

import Data.Aeson
import Data.JSONAPI.Internal.Meta
import Control.Monad (forM)
import qualified Data.HashMap.Strict as HS
import qualified JavaScript.Object.Internal as OI
import           Control.Monad.Trans.Maybe
-- import           GHCJS.Foreign                hiding (Object, String)
-- import           GHCJS.Foreign.Internal (JSONType(..))
-- import           GHCJS.JSVal.Combinators
-- import qualified Data.HashMap.Strict as HM
import           GHCJS.Marshal
-- import           GHCJS.Prim
import           GHCJS.Types
-- import qualified Data.JSString as JSS
import Data.JSONAPI.Internal.GHCJS.Fix (listProps)
import qualified Data.JSString.Text as JSS


instance ToJSVal Object where
  toJSVal = toJSVal_aeson
  {-# INLINE toJSVal #-}

instance FromJSVal Object where
  fromJSVal r = do 
    props <- listProps (OI.Object r)
    runMaybeT $ do
        propVals <- forM props $ \p -> do
            v <- MaybeT (fromJSVal =<< OI.getProp p (OI.Object r))
            return (JSS.textFromJSString p, v)
        return ( (HS.fromList propVals))

instance ToJSVal Meta where
  toJSVal (Meta o) = toJSVal o

instance FromJSVal Meta where
  fromJSVal v = fmap Meta <$> fromJSVal v

-- | Helper to implement 'FromJSVal' via aeson instance
fromJSVal_aeson :: FromJSON a => JSVal -> IO (Maybe a)
fromJSVal_aeson v = do
  jsn <- fromJSVal v
  return $ parse =<< jsn
  where
  parse jsn = case fromJSON jsn of 
    Error   _ -> Nothing 
    Success a -> Just a       
