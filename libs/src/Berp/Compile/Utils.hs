-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.Utils
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Utilities that have no better home.
--
-----------------------------------------------------------------------------

module Berp.Compile.Utils where

unsupported :: String -> a
unsupported str = error $ "berp unsupported. " ++ str
