-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Mangle
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Name mangling. 
--
-- We need to mangle Python's identifier names when we compiled them to 
-- Haskell names because:
--    1) Python allows some identifiers which are illegal in Haskell,
--       such as leading upper case letters.
--    2) We introduce our own "temporary" variables into a compiled program.
--       Name mangling helps to avoid accidental name clash.
--    3) We import many Haskell primitives into the compiled program. 
--       Name mangling helps to avoid accidental name clash.
--
-----------------------------------------------------------------------------

module Berp.Base.Mangle (mangle, deMangle) where

import Data.List (isPrefixOf)

sourcePrefix = "_s_"

mangle :: String -> String
mangle = (sourcePrefix ++)

deMangle :: String -> String
deMangle str
   | sourcePrefix `isPrefixOf` str = drop (length sourcePrefix) str
   | otherwise = str
