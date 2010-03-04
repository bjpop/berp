{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Function (function, functionClass) where

import Berp.Base.Env (methodsFromList, emptyVarEnv)
import Berp.Base.Monad (constant)
import Berp.Base.SemanticTypes (Object (..), Procedure, VarEnv)
import Berp.Base.StdTypes.None (none)
import Berp.Base.Monad (constant)
import Berp.Base.Identity (newIdentity)
import Berp.Base.StdTypes.Object (objectClass)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDict)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

{-# NOINLINE function #-}
function :: Int -> Procedure -> Object 
function arity p = constant $ do 
   identity <- newIdentity
   dict <- emptyDict
   return $
      Function 
      { object_identity = identity 
      , object_dict = dict 
      , object_procedure = p
      , object_arity = arity
      }

{-# NOINLINE functionClass #-}
functionClass :: Object
functionClass = constant $ do 
   as <- attributes
   identity <- newIdentity 
   dict <- attributes
   return $
      Type 
      { object_identity = identity 
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = objectBase 
      , object_constructor = error "function type does not provide a constructor" 
      , object_type_name = string "function" 
      }

-- XXX update my attributes
attributes :: IO Object 
attributes = mkAttributes []
