module Data.JSONAPI.Internal.Util where

import Data.Aeson
import Data.Aeson.Types (Pair)
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

(.=#) :: ToJSON b => Text -> HM.HashMap Text b -> [Pair]
(.=#) name hmValue =
  case length lValue of
    0 -> []
    _ -> [name .= object ((\(x,y) -> (x,toJSON y)) <$> lValue)]
  where 
    lValue = HM.toList hmValue
infixr 8 .=#
