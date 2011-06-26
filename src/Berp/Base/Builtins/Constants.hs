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
   (object, _s_object, _s_type, _s_bool, _s_set)
   where

import Berp.Base.SemanticTypes (ObjectRef)
import Berp.Base.Builtins.Utils (primConstant)
import {-# SOURCE #-} Berp.Base.StdTypes.Object (object)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (boolClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Set (setClass)

_s_object :: ObjectRef
_s_object = primConstant object

_s_type :: ObjectRef
_s_type = primConstant typeClass

_s_bool :: ObjectRef
_s_bool = primConstant boolClass

_s_set :: ObjectRef
_s_set = primConstant setClass
