{-# OPTIONS_GHC -XTemplateHaskell -XPatternGuards #-}
module Berp.Base.StdTypes.Type (typeClass) where

import Control.Monad.Trans (liftIO)
import Berp.Base.SemanticTypes (Object (..), Eval, Procedure)
import Berp.Base.Monad (constant)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Env (emptyVarEnv)
import Berp.Base.StdTypes.Object (objectClass)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Hash (hashedStr)
import Berp.Base.Object (typeOf)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDict)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

{-# NOINLINE typeClass #-}
typeClass :: Object
typeClass = constant $ do 
   identity <- newIdentity
   dict <- attributes
   return $ 
      Type 
      { object_identity = identity 
      , object_type = typeClass  -- yes it is recursive!
      , object_dict = dict
      , object_bases = objectBase 
      , object_constructor = newType 
      , object_type_name = string "type"
      }

newType :: Procedure
newType args
   | [obj] <- args = return $ typeOf obj 
   | [name, bases, dict] <- args = liftIO $ do
        identity <- newIdentity
        return $ 
           Type 
           { object_identity = identity
           , object_type = typeClass
           , object_dict = dict
           , object_bases = bases
           , object_constructor = newType -- XXX this should create an object, not a type?
           , object_type_name = name 
           }  
   | otherwise = fail "type() takes 1 or 3 arguments"

attributes :: IO Object
attributes = mkAttributes []
