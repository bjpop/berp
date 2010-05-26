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
   (_s_object, _s_type)
   where

import Berp.Base.SemanticTypes (ObjectRef)
import Berp.Base.Builtins.Utils (primConstant)
import Berp.Base.StdTypes.Object (object)
import Berp.Base.StdTypes.Type (typeClass)

_s_object :: ObjectRef              
_s_object = primConstant object

_s_type :: ObjectRef
_s_type = primConstant typeClass
