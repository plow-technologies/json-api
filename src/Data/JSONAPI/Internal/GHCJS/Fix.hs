module Data.JSONAPI.Internal.GHCJS.Fix where

import Data.JSString
import           JavaScript.Array.Internal (JSArray, SomeJSArray(..), toListIO)  
import           JavaScript.Object.Internal (Object)
import           Unsafe.Coerce

listProps :: Object -> IO [JSString]
listProps o = do
  ps <- js_listProps o
  l <- toListIO ps
  return $ unsafeCoerce l
{-# INLINE listProps #-}

foreign import javascript unsafe  "h$listProps"
  js_listProps :: Object -> IO JSArray
