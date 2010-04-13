{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Object (object) where

import Control.Monad.Trans (liftIO)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval)
import Berp.Base.Monad (constantIO)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import Berp.Base.Prims (primitive)
import Berp.Base.Object (identityOf)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDictionary)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (emptyTuple)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

{-# NOINLINE object #-}
object :: Object
object = constantIO $ do 
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
   dict <- emptyDictionary
   return $
      Object
      { object_identity = identity 
      , object_type = object
      , object_dict = dict 
      }

attributes :: IO Object
attributes = mkAttributes 
   [ (strName, primitive 1 str) ]

str :: Procedure
str (x:_) = 
   case x of 
      Object {} -> do
         let objTypeNameStr = object_string $ object_type_name $ object_type x
         let identity = identityOf x 
         return $ string $ "<" ++ objTypeNameStr ++ " object with identity " ++ show identity ++ ">" 
      Type {} -> do
         let typeName = object_string $ object_type_name x
         return $ string $ "<class " ++ typeName ++ ">" 
      -- This should never happen because all other object types have a specialised
      -- str method.
      other -> return $ string "<unknown object>"
