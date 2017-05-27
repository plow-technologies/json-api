module Data.JSONAPI.Internal.Util where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

(.=?) :: ToJSON v => Text -> Maybe v -> [Pair]
(.=?) name mValue =
  case mValue of
    Nothing    -> []
    Just value -> [name .= value]
infixr 8 .=?

(.=@) :: ToJSON a => Text -> [a] -> [Pair]
(.=@) name lValue =
  case length lValue of
    0 -> []
    1 -> [name .= (head lValue)]
    _ -> [name .= lValue]
infixr 8 .=@

(.=@!) :: ToJSON a => Text -> [a] -> [Pair]
(.=@!) name lValue =
  case length lValue of
    0 -> []
    _ -> [name .= lValue]
infixr 8 .=@!

(.=#) :: ToJSON b => Text -> HM.HashMap Text b -> [Pair]
(.=#) name hmValue =
  case length lValue of
    0 -> []
    _ -> [name .= object ((\(x,y) -> (x,toJSON y)) <$> lValue)]
  where 
    lValue = HM.toList hmValue
infixr 8 .=#

-- | Looking for a single object, an array or objects, or if the key does not 
-- exist then return an empty list.
(.:@) :: FromJSON a => Object -> Text -> Parser [a]
(.:@) o key = 
  case HM.lookup key o of
    Just arr@(Array  _a) -> parseJSON arr
    Just obj@(Object _o) -> (:[]) <$> parseJSON obj
    _                    -> pure []
