{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Object (objectClass) where

import Control.Monad.Trans (liftIO)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval)
import Berp.Base.Monad (constant)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Env (emptyVarEnv)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDict)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (emptyTuple)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

{-# NOINLINE objectClass #-}
objectClass :: Object
objectClass = constant $ do 
   identity <- newIdentity
   dict <- attributes 
   return $ 
      Type 
      { object_identity = identity 
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = emptyTuple -- this should be empty! it is not a mistake.
      , object_constructor = newObject 
      , object_type_name = string "object"
      }

newObject :: Procedure
newObject _ = liftIO $ do
   identity <- newIdentity
   dict <- emptyDict
   return $
      Object
      { object_identity = identity 
      , object_type = objectClass 
      , object_dict = dict 
      }

attributes :: IO Object
attributes = mkAttributes []
