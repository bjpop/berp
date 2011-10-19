-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Generator
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard generator type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Generator (generator, generatorClass) where

import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import Berp.Base.Prims (generatorNext, primitive)
import Berp.Base.LiftedIO (newIORef)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Function (function)

generator :: Eval Object -> Eval Object
generator continuation = do
   identity <- newIdentity
   contRef <- newIORef continuation
   stackRef <- newIORef id
   return $
      Generator
      { object_identity = identity
      , object_continuation = contRef
      , object_stack_context = stackRef
      }

generatorClass :: Eval Object
generatorClass = do
   dict <- attributes
   base <- objectBase
   newType [string "generator", base, dict]

-- XXX update my attributes
attributes :: Eval Object
attributes = mkAttributesList
   [ (specialIterName, iter)
   , (specialStrName, str)
   , (specialNextName, next)
   ]

-- XXX fixme
str :: Eval Object
str = primitive 1 $ \_ -> return $ string $ "Generator"

iter :: Eval Object
iter = primitive 1 fun
   where
   fun (x:_) = return x
   fun _other = error "iter method on generator applied to the wrong number of arguments"

next :: Eval Object
next = function 1 generatorNext {- Nothing -}
