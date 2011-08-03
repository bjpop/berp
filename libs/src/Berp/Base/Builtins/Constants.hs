-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Builtins.Constants
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Constant builtin values (typically constant objects).
--
-----------------------------------------------------------------------------

module Berp.Base.Builtins.Constants
   (object)
   where

import Berp.Base.SemanticTypes (ObjectRef)
import Berp.Base.Builtins.Utils (primConstant)
import {-# SOURCE #-} Berp.Base.StdTypes.Object (object)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (boolClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Set as Set (setClass, set)

