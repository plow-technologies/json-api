module Data.JSONAPI.Internal.Util where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Text (Text)


-- avoid {"key":null} if Maybe is Nothing
mkPairOnlyIfValueExists :: ToJSON v => Text -> Maybe v -> [Pair]
mkPairOnlyIfValueExists = (.=?)

(.=?) :: ToJSON v => Text -> Maybe v -> [Pair]
(.=?) name mValue =
  case mValue of
    Nothing    -> []
    Just value -> [name .= value]
infixr 8 .=?

-- avoid {"key":null} if List is [], if list is of length 1 then {"key":object},
-- if greater than one than {key:[object]}
mkPairAsSingleObjectOrList :: ToJSON a => Text -> [a] -> [Pair]
mkPairAsSingleObjectOrList = (.=@)

(.=@) :: ToJSON a => Text -> [a] -> [Pair]
(.=@) name lValue =
  case length lValue of
    0 -> []
    1 -> [name .= (head lValue)]
    _ -> [name .= lValue]
infixr 8 .=@

mkPairOnlyIfListNotEmpty :: ToJSON a => Text -> [a] -> [Pair]
mkPairOnlyIfListNotEmpty = (.=@!)

-- avoid {key:null} if List is []
(.=@!) :: ToJSON a => Text -> [a] -> [Pair]
(.=@!) name lValue =
  case length lValue of
    0 -> []
    _ -> [name .= (toJSON <$> lValue)]
infixr 8 .=@!

-- avoid {key:null} if HashMap is []
mkPairOnlyIfHashMapNotEmpty :: ToJSON b => Text -> HM.HashMap Text b -> [Pair]
mkPairOnlyIfHashMapNotEmpty = (.=#)

(.=#) :: ToJSON b => Text -> HM.HashMap Text b -> [Pair]
(.=#) name hmValue =
  case length lValue of
    0 -> []
    _ -> [name .= object ((\(x,y) -> (x,toJSON y)) <$> lValue)]
  where 
    lValue = HM.toList hmValue
infixr 8 .=#

-- | Look for a single object or an array of objects. If the key does not 
-- exist then return an empty list.
parseSingleObjectOrArrayAsList :: (Eq a, FromJSON a) => Object -> Text -> Parser [a]
parseSingleObjectOrArrayAsList = (.:@)


(.:@) :: (Eq a, FromJSON a) => Object -> Text -> Parser [a]
(.:@) o key = 
  case HM.lookup key o of
    Just arr@(Array  _a) -> nub <$> parseJSON arr
    Just obj@(Object _o) -> (:[]) <$> parseJSON obj
    _                    -> pure []
