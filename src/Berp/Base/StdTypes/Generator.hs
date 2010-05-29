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

import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
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

{-# NOINLINE generatorClass #-}
generatorClass :: Object
generatorClass = constantIO $ do 
   dict <- attributes
   newType [string "generator", objectBase, dict]

-- XXX update my attributes
attributes :: IO Object
attributes = mkAttributes
   [ (iterName, iter)
   , (strName, str)
   , (nextName, next)
   ]

-- XXX fixme
str :: Object
str = primitive 1 $ \_ -> return $ string $ "Generator" 

iter :: Object
iter = primitive 1 $ \(x:_) -> return x

next :: Object
next = function 1 generatorNext 
