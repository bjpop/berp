{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Dictionary (emptyDict, dict, dictClass) where

import Control.Monad.Trans (liftIO)
import Berp.Base.Prims (primitive)
import Berp.Base.Monad (constant)
import Berp.Base.Env (VarEnv, methodsFromList)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Object (objectClass)
import Berp.Base.Identity (newIdentity)
import Berp.Base.HashTable as Hash (fromList, empty)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
-- import {-# SOURCE #-} Berp.Base.StdTypes.Primitive (primitive)

emptyDict :: IO Object
emptyDict = do 
   identity <- newIdentity
   hashTable <- Hash.empty 
   return $ 
      Dictionary 
      { object_identity = identity
      , object_hashTable = hashTable 
      }

dict :: [(Object, Object)] -> Eval Object
dict elements = do 
   identity <- liftIO $ newIdentity
   hashTable <- fromList elements
   return $ 
      Dictionary 
      { object_identity = identity
      , object_hashTable = hashTable 
      }

{-# NOINLINE dictClass #-}
dictClass :: Object
dictClass = constant $ do 
   identity <- newIdentity
   dict <- attributes
   return $
      Type 
      { object_identity = identity
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = objectBase
      , object_constructor = \_ -> liftIO emptyDict  
      , object_type_name = string "dict"
      }

attributes :: IO Object 
attributes = mkAttributes 
   [ (eqName, primitive 2 eq)
   , (strName, primitive 1 str)
   ]

eq :: Procedure 
eq = error "== on dict not defined"

str :: Procedure 
str = error "str on dict not defined" 
