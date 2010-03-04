-- {-# OPTIONS_GHC -cpp -DDEBUG #-} 
{-# OPTIONS_GHC -cpp #-}
-- uncomment one of the two above lines to turn debugging on/off for this module
#include "BerpDebug.h"

module Berp.Base.Object 
   (lookupAttribute, lookupAttributeMaybe, 
    typeOf, identityOf, hasAttribute) where

import Berp.Base.Ident
import Data.Map as Map (lookup)
import Data.IORef (readIORef)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Berp.Base.SemanticTypes (ObjectRef, Object (..), Eval, VarEnv)
import Berp.Base.Mangle (deMangle)
import Berp.Base.Monad (constant)
import Berp.Base.Identity (Identity)
import Berp.Base.Hash (Hashed)
import {-# SOURCE #-} Berp.Base.HashTable (stringLookup)
import {-# SOURCE #-} Berp.Base.StdTypes.Integer (intClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (boolClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (tupleClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (functionClass)
import {-# SOURCE #-} Berp.Base.StdTypes.String (stringClass)
import {-# SOURCE #-} Berp.Base.StdTypes.None (noneClass, noneIdentity)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (dictClass)
import {-# SOURCE #-} Berp.Base.StdTypes.List (listClass)
-- import {-# SOURCE #-} Berp.Base.StdTypes.Primitive (primitiveClass)

-- Python allows the type of an object to change in a limited set of circumstances.
-- But we will ignore that for the moment and make it a pure function.
typeOf :: Object -> Object
typeOf obj@(Object {}) = object_type obj 
typeOf obj@(Type {}) = object_type obj
typeOf (Integer {}) = intClass 
typeOf (Bool {}) = boolClass
typeOf (Tuple {}) = tupleClass
typeOf (List {}) = listClass 
typeOf (Function {}) = functionClass
typeOf (String {}) = stringClass
typeOf (None {}) = noneClass
typeOf (Dictionary {}) = dictClass
typeOf (List {}) = listClass
-- typeOf (Primitive {}) = primitiveClass

-- The identity of an object should never change so this can be a pure function.
identityOf :: Object -> Identity
identityOf None = noneIdentity 
identityOf object = object_identity object

dictOf :: Object -> Maybe Object 
dictOf obj@(Object {}) = Just $ object_dict obj
dictOf obj@(Type {}) = Just $ object_dict obj
dictOf obj@(Function {}) = Just $ object_dict obj
dictOf other = Nothing

lookupAttribute :: Object -> Hashed String -> IO Object
lookupAttribute obj ident@(_, identStr) = do
   maybeObj <- lookupAttributeMaybe obj ident
   case maybeObj of
      -- XXX This should raise a proper catchable exception 
      Nothing -> fail $ show obj ++ " has no attribute called " ++ deMangle identStr
      Just attributeObj -> case attributeObj of
         -- XXX this should return a bound method object
         Function { object_procedure = proc, object_arity = arity } -> 
            return attributeObj { object_procedure = \args -> proc (obj:args), object_arity = arity - 1 }
         _other -> return attributeObj 

-- XXX does not handle descriptors or getattr/getattribute.
-- XXX MRO needs to be supported.
-- XXX Hack: If the result of the lookup is a function, then 
--     turn it into a bound method on return, by supplying the
--     object as the first argument. This is not ideal, but
--     it will work until descriptors are supported
lookupAttributeMaybe :: Object -> Hashed String -> IO (Maybe Object)
lookupAttributeMaybe object ident = do
   BELCH("Looking for: " ++ show ident ++ " in: " ++ show object)
   BELCH("Looking in dictionary of object")
   case dictOf object of
      -- The object does have a dictionary; look in there.
      Just dict -> do
         BELCH("Object has a dictionary")
         dictResult <- stringLookup ident $ object_hashTable dict 
         case dictResult of
            -- The ident was not found in the object, look in the type, then the bases.
            Nothing -> do
               BELCH("Ident not found in dictionary of object")
               lookupAttributeType 
            -- The ident was found in the object; return it.
            Just _ -> do
               BELCH("Ident was found in dictionary of object")
               return dictResult 
      -- The object does not have a dictionary; look in the type, then the bases.
      Nothing -> do
         BELCH("Object does not have a dictionary")
         lookupAttributeType
   where
   objectType :: Object
   objectType = typeOf object
   lookupAttributeType :: IO (Maybe Object)
   lookupAttributeType = do
      BELCH("Looking in dict of the type: " ++ show objectType)
      case dictOf objectType of
         -- The type of the object has no dictionary (weird).
         Nothing -> do
            BELCH("Type does not have a dictionary")
            lookupAttributeBases
         -- The type of the object has a dictionary; look in there.
         Just typeDict -> do
            BELCH("Type does have a dictionary")
            typeDictResult <- stringLookup ident $ object_hashTable typeDict 
            case typeDictResult of
               -- The ident was not found in the type of the object, look in the bases.
               Nothing -> do
                  BELCH("Ident not found in dictionary of type")
                  lookupAttributeBases
               -- The ident was found in the type of the object; return it.
               Just _ -> do
                  BELCH("Ident was found in dictionary of type")
                  return typeDictResult 
   -- Look in the dictionaries of all the bases of the object
   -- XXX we should look in the bases of bases recursively if necessary
   lookupAttributeBases :: IO (Maybe Object)
   lookupAttributeBases = do
      -- XXX this could fail badly if the type of the object is malformed.
      -- we could check for the proper form of the type but that would slow us down
      -- and this code will be called often.
      BELCH("Looking in the bases of the type of the object")
      let baseObjects = object_tuple $ object_bases objectType
      loopOverBases baseObjects
      where
      loopOverBases :: [Object] -> IO (Maybe Object)
      loopOverBases [] = do
         BELCH("Ident was not found in the bases of the type of the object")
         return Nothing
      loopOverBases (base:rest) = do
         BELCH("Looking in the dict of the base: " ++ show base)
         case dictOf base of
            Nothing -> do
               BELCH("Base does not have a dictionary")
               loopOverBases rest 
            Just dict -> do
               BELCH("Base does have a dictionary")
               dictResult <- stringLookup ident $ object_hashTable dict
               case dictResult of
                  Nothing -> do
                     BELCH("Ident not found in dictionary of base")
                     loopOverBases rest 
                  Just _ -> do
                     BELCH("Ident was found in dictionary of base")
                     return dictResult

hasAttribute :: Object -> Hashed String -> IO Bool
hasAttribute object ident = isJust <$> lookupAttributeMaybe object ident
