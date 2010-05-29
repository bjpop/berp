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

module Berp.Base.Builtins.Utils (primFun, primConstant) where

import Data.IORef (newIORef)
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Arity, Procedure, ObjectRef, Object)
import Berp.Base.Prims (primitive)
import Berp.Base.Monad (constantIO) 

primFun :: Ident -> Arity -> Procedure -> ObjectRef 
primFun _ident arity proc = constantIO $ do
   let primitiveObj = primitive arity proc 
   newIORef primitiveObj 

primConstant :: Object -> ObjectRef
primConstant = constantIO . newIORef
