{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec (hspec)

#if defined(ghcjs_HOST_OS)
import qualified GHCJS.Spec as GHCJS
#else
import qualified DocumentExamples as DE
import qualified GHC.Spec as GHC
#endif

main :: IO ()
main = do
#if defined(ghcjs_HOST_OS)
  hspec GHCJS.spec
#else
  hspec DE.spec
  hspec GHC.spec
#endif