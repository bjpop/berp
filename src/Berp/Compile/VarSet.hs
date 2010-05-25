-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.VarSet
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A set of variables. XXX Does this need to be in its own module?
--
-----------------------------------------------------------------------------

module Berp.Compile.VarSet where

import Data.Set
import Berp.Compile.IdentString

type VarSet = Set IdentString 
