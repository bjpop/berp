-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Builtins.Utils
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Support code for making builtins.
--
-----------------------------------------------------------------------------

module Berp.Base.Builtins.Utils (primFun) where

import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Arity, Procedure, ObjectRef, Eval)
import Berp.Base.Prims (primitive)
import Berp.Base.LiftedIO (newIORef)

primFun :: Ident -> Arity -> Procedure -> Eval ObjectRef
primFun _ident arity proc = do
   primitiveObj <- primitive arity proc
   newIORef primitiveObj
