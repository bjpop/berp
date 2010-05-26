-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.ObjectBase
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Most (all?) of the standard types have "object" as their (only) base
-- class. In Python the base classes are stored as a tuple of objects.
-- Since this is shared by many of the standard types it makes sense
-- to define it once, instead of making many copies.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.ObjectBase (objectBase) where

import Berp.Base.SemanticTypes (Object)
import {-# SOURCE #-} Berp.Base.StdTypes.Object (object)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (tuple)

objectBase :: Object
objectBase = tuple [object]
