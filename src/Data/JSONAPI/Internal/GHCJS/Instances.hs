{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.JSONAPI.Internal.GHCJS.Instances where

import Data.Aeson
import Data.JSONAPI.Internal.Meta
import Control.Monad (forM)
import qualified Data.HashMap.Strict as H
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
import qualified Data.JSString.Text as JSS

instance ToJSVal Object where
  toJSVal o = toJSVal_aeson o

instance FromJSVal Object where
  fromJSVal r = do 
    props <- OI.listProps (OI.Object r)
    runMaybeT $ do
        propVals <- forM props $ \p -> do
            v <- MaybeT (fromJSVal =<< OI.getProp p (OI.Object r))
            return (JSS.textFromJSString p, v)
        return ( (H.fromList propVals))
    
--instance FromJSVal 
instance ToJSVal Meta where
  toJSVal (Meta o) = toJSVal o

instance FromJSVal Meta where
  fromJSVal v = fmap Meta <$> fromJSVal v

{-
    mj <- fromJSVal v :: IO (Maybe Value)
    case mj of
      Just (Object j) -> return $ Just $ Meta j
      Just _          -> return $ Just $ Meta HM.empty
      _ -> return Nothing
-}

-- | Helper to implement 'FromJSVal' via aeson instance
fromJSVal_aeson :: FromJSON a => JSVal -> IO (Maybe a)
fromJSVal_aeson v = do
  jsn <- fromJSVal v
  return $ parse =<< jsn
  where
  parse jsn = case fromJSON jsn of 
    Error   _ -> Nothing 
    Success a -> Just a       
{-
instance ToJSON Meta where
  toJSON (Meta o) = object $ HM.toList o

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \o -> return $ Meta o

instance FromJSVal ByteString where
  fromJSVal v = do
    mv <- fromJSVal v :: IO (Maybe Text)
    case mv of
      Nothing -> return Nothing
Just b -> return $ decode . encode $ b

instance FromJSVal Pico where
  fromJSVal r =
    case jsonTypeOf r of
      JSONString  -> (fmap read) <$> (fromJSVal r :: (IO (Maybe String)))
_ -> return Nothing

instance FromJSVal AE.Value where
    fromJSVal r = case jsonTypeOf r of
            JSONNull    -> return (Just AE.Null)
            JSONInteger -> liftM (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer))
                 <$> fromJSVal r
            JSONFloat   -> liftM (AE.Number . (fromFloatDigits :: Double -> Scientific))
                 <$> fromJSVal r
            JSONBool    -> liftM AE.Bool  <$> fromJSVal r
            JSONString  -> liftM AE.String <$> fromJSVal r
            JSONArray   -> liftM (AE.Array . V.fromList) <$> fromJSVal r
JSONObject -> do
-}
