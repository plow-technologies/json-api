{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.JSONAPI.Internal.GHCJS.Instances where

import Data.Aeson
import Data.JSONAPI.Internal.Meta

-- import           Control.Monad.Trans.Maybe (runMaybeT)
-- import           GHCJS.Foreign                hiding (Object, String)
-- import           GHCJS.Foreign.Internal (JSONType(..))
-- import           GHCJS.JSVal.Combinators
import qualified Data.HashMap.Strict as HM
import           GHCJS.Marshal
-- import           GHCJS.Prim
-- import           GHCJS.Types

instance ToJSVal Meta where
  toJSVal (Meta o) = toJSVal_aeson o

instance FromJSVal Meta where
  fromJSVal v = do
    mj <- fromJSVal v :: IO (Maybe Value)
    case mj of
      Just (Object j) -> return $ Just $ Meta j
      Just _          -> return $ Just $ Meta HM.empty
      _ -> return Nothing
      
{-
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
