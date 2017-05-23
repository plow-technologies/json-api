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
(.:@) :: (FromJSON a) => Object -> Text -> Parser [a]
obj .:@ key = case HM.lookup key obj of
                Nothing -> pure []
                Just  v -> parseJSON v <?> 
-}

(.=@) :: ToJSON a => Text -> [a] -> [Pair]
(.=@) name lValue =
  case length lValue of
    0 -> []
    1 -> [name .= (head lValue)]
    _ -> [name .= lValue]
infixr 8 .=@