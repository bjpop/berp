-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Version
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The current version of berp, exported as a Haskell string.
--
-----------------------------------------------------------------------------

module Berp.Version (version, versionString) where

import Paths_berp (version)
import Data.Version (showVersion)

versionString :: String
versionString = showVersion version 
