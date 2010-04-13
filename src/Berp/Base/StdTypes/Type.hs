{-# OPTIONS_GHC -XTemplateHaskell -XPatternGuards #-}
module Berp.Base.StdTypes.Type (typeClass, newType) where

import Control.Monad.Trans (liftIO)
import Berp.Base.SemanticTypes (Object (..), Eval, Procedure)
import Berp.Base.Monad (constantIO)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Hash (hashedStr)
import Berp.Base.Object (typeOf)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDictionary)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

{-# NOINLINE typeClass #-}
typeClass :: Object
typeClass = constantIO $ do 
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
        let theType =
             Type 
             { object_identity = identity
             , object_type = typeClass
             , object_dict = dict
             , object_bases = bases
             , object_constructor = instantiate theType 
             , object_type_name = name 
             }  
        return theType 
   | otherwise = fail "type() takes 1 or 3 arguments"

instantiate :: Object -> Procedure
instantiate objectType _ = liftIO $ do
   identity <- newIdentity
   dict <- emptyDictionary
   return $
      Object
      { object_identity = identity
      , object_type = objectType 
      , object_dict = dict
      }

attributes :: IO Object
attributes = mkAttributes []
