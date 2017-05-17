module Data.JSONAPI.Internal.Util where

import Data.Aeson ((.=), ToJSON)
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

(.=?) :: ToJSON v => Text -> Maybe v -> [Pair]
(.=?) name mValue =
  case mValue of
    Nothing    -> []
    Just value -> [name .= value]
infixr 8 .=?

{-
mmmm :: (ToJSON k, ToJSON v) => Text -> HM.HashMap k v -> [Pair]
mmmm name hm =
  case HM.size hm of
    0 -> []
    1 -> 
-}