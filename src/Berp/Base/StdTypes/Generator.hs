{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Generator (generator, generatorClass) where

import Control.Monad.Trans (liftIO)
import Data.IORef (newIORef)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Object (..), Procedure, Eval)
import Berp.Base.StdTypes.None (none)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import Berp.Base.Prims (generatorNext, primitive)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (function)

generator :: Eval Object -> Eval Object 
generator continuation = liftIO $ do 
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
   as <- attributes
   identity <- newIdentity 
   dict <- attributes
   return $
      Type 
      { object_identity = identity 
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = objectBase 
      , object_constructor = \_ -> error "generator type does not provide a constructor" 
      , object_type_name = string "generator" 
      }

-- XXX update my attributes
attributes :: IO Object
attributes = mkAttributes
   [ (iterName, iter)
   , (strName, str)
   , (nextName, next)
   ]

-- XXX fixme
str :: Object
str = primitive 1 $ \[x] -> return $ string $ "Generator" 

iter :: Object
iter = primitive 1 $ \[x] -> return x

next :: Object
next = function 1 generatorNext 
