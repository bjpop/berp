-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Object
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Primitive operations on Objects.
--
-----------------------------------------------------------------------------

#include "BerpDebug.h"

module Berp.Base.Object
   ( lookupAttribute, lookupSpecialAttribute, lookupAttributeMaybe
   , typeOf, identityOf, objectEquality, dictOf, dir
   , isIterator
   ) where

import Berp.Base.Truth (truth)
import Berp.Base.Prims (callMethod, showObject, lookupBuiltin)
import Data.List (nub)
import Control.Monad (zipWithM)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, catMaybes)
import Berp.Base.SemanticTypes (Object (..), Eval, Identity (..))
import Berp.Base.Mangle (deMangle)
import Berp.Base.Hash (Hashed)
import Berp.Base.StdNames (specialEqName, specialCmpName, specialIterName)
#ifdef DEBUG
import Berp.Base.LiftedIO as LIO (putStrLn)
#endif
import {-# SOURCE #-} Berp.Base.HashTable (stringLookup, keys)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (getTupleElements)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.List (list)

-- Python allows the type of an object to change in a limited set of circumstances.
-- But we will ignore that for the moment and make it a pure function.
-- XXX should really hash the strings here, so that they get done once, not repeatedly
typeOf :: Object -> Eval Object
typeOf obj@(Object {}) = return $ object_type obj
typeOf obj@(Type {}) = return $ object_type obj
typeOf (Integer {}) = lookupBuiltin "int"
typeOf (Float {}) = lookupBuiltin "float"
typeOf (Complex {}) = lookupBuiltin "complexClass"
typeOf (TrueObject {}) = lookupBuiltin "bool"
typeOf (FalseObject {}) = lookupBuiltin "bool"
typeOf (Tuple {}) = lookupBuiltin "tuple"
typeOf (List {}) = lookupBuiltin "list"
typeOf (Function {}) = lookupBuiltin "function"
typeOf (String {}) = lookupBuiltin "str"
typeOf (None {}) = lookupBuiltin "none"
typeOf (Dictionary {}) = lookupBuiltin "dict"
typeOf (Generator {}) = lookupBuiltin "generator"
typeOf (Set {}) = lookupBuiltin "setClass"
typeOf (Module {}) = lookupBuiltin "module"
typeOf (IdentityObject {}) = lookupBuiltin "id"

-- The identity of an object should never change so this can be a pure function.
{-
identityOf :: Object -> Identity
identityOf None = noneIdentity
identityOf object = object_identity object
-}
identityOf :: Object -> Identity
identityOf (Integer { object_integer = i }) = IntegerID i
identityOf (Float { object_float = f }) = FloatID f
identityOf (Complex { object_complex = c }) = ComplexID c
identityOf (String { object_string = s }) = StringID s
identityOf TrueObject = TrueID
identityOf FalseObject = FalseID
identityOf None = NoneID
identityOf object = object_identity object

dictOf :: Object -> Maybe Object
dictOf obj@(Object {}) = Just $ object_dict obj
dictOf obj@(Type {}) = Just $ object_dict obj
dictOf obj@(Function {}) = Just $ object_dict obj
dictOf obj@(Module {}) = Just $ object_dict obj
dictOf _other = Nothing

lookupAttribute :: Object -> Hashed String -> Eval Object
lookupAttribute obj ident = do
   lookupResult <- lookupAttributeMaybe obj ident
   checkLookup obj ident lookupResult

lookupSpecialAttribute :: Object -> Hashed String -> Eval Object
lookupSpecialAttribute obj ident = do
   lookupResult <- lookupSpecialAttributeMaybe obj ident
   checkLookup obj ident lookupResult

checkLookup :: Object -> Hashed String -> Maybe Object -> Eval Object
checkLookup obj (_, identStr) lookupResult =
   case lookupResult of
      -- XXX This should raise a proper catchable exception 
      Nothing -> do
         objStr <- showObject obj
         fail $ objStr ++ " has no attribute called " ++ deMangle identStr
      Just attributeObj -> do
         case attributeObj of
            -- XXX this should return a bound method object
            Function { object_procedure = proc, object_arity = arity } -> 
               return attributeObj { object_procedure = \args -> proc (obj:args), object_arity = arity - 1 }
            _other -> do
               return attributeObj 

-- XXX does not handle descriptors or getattr/getattribute.
-- XXX Hack: If the result of the lookup is a function, then 
--     turn it into a bound method on return, by supplying the
--     object as the first argument. This is not ideal, but
--     it will work until descriptors are supported

lookupSpecialAttributeMaybe :: Object -> Hashed String -> Eval (Maybe Object)
lookupSpecialAttributeMaybe object ident = do
   BELCH("Looking for special attribute: " ++ show ident ++ " in: " ++ show object)
   lookupAttributeType object ident

lookupAttributeMaybe :: Object -> Hashed String -> Eval (Maybe Object)
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
               lookupAttributeType object ident
            -- The ident was found in the object; return it.
            Just _ -> do
               BELCH("Ident was found in dictionary of object")
               return dictResult 
      -- The object does not have a dictionary; look in the type, then the bases.
      Nothing -> do
         BELCH("Object does not have a dictionary")
         lookupAttributeType object ident

lookupAttributeType :: Object -> Hashed String -> Eval (Maybe Object)
lookupAttributeType object ident = do
   objectType <- typeOf object
   BELCH("Looking in dict of the type: " ++ show objectType)
   let mroList = getTupleElements $ object_mro objectType
   searchMRO mroList
   where
   searchMRO :: [Object] -> Eval (Maybe Object)
   searchMRO [] = do
      BELCH("Ident was not found in the mro of the type of the object")
      return Nothing
   searchMRO (klass:rest) = do
      BELCH("Looking in the dict of the type: " ++ show klass)
      case dictOf klass of
         Nothing -> do
            BELCH("Type does not have a dictionary")
            searchMRO rest 
         Just dict -> do
            BELCH("Type does have a dictionary")
            dictResult <- stringLookup ident $ object_hashTable dict
            case dictResult of
               Nothing -> do
                  BELCH("Ident not found in dictionary of type")
                  searchMRO rest 
               Just _ -> do
                  BELCH("Ident was found in dictionary of type")
                  return dictResult

-- | Check if two objects are equal. For some objects we might have
--   to call the __eq__ (or __cmp__) method on the objects. This means
--   the result must be in the Eval monad.
objectEquality :: Object -> Object -> Eval Bool
objectEquality obj1@(Integer {}) obj2@(Integer {})
   = return (object_integer obj1 == object_integer obj2)
objectEquality (TrueObject {}) (TrueObject {})
   = return True
objectEquality (TrueObject {}) _object
   = return False
objectEquality  _object (TrueObject {})
   = return False
objectEquality (FalseObject {}) (FalseObject {})
   = return True
objectEquality (FalseObject {}) _object
   = return False
objectEquality  _object (FalseObject {})
   = return False
objectEquality obj1@(Tuple {}) obj2@(Tuple {})
   | object_identity obj1 == object_identity obj2 = return True
   | object_length obj1 == object_length obj2 =
        and <$> zipWithM objectEquality (object_tuple obj1) (object_tuple obj2)
   | otherwise = return False
objectEquality obj1@(String {}) obj2@(String {})
   = return (object_string obj1 == object_string obj2)
objectEquality None None = return True
objectEquality obj1 obj2
   | object_identity obj1 == object_identity obj2 = return True
   | otherwise = do
        canEq <- hasAttribute specialEqName obj1
        if canEq
           then do
              truthObj <- callMethod obj1 specialEqName [obj2]
              truth truthObj
           else do
              canCmp <- hasAttribute specialCmpName obj1
              if canCmp
                 then do
                    cmpResult <- callMethod obj1 specialCmpName [obj2]
                    case cmpResult of
                       Integer {} -> return $ object_integer cmpResult == 0
                       _other -> fail $ "__cmp__ method on object does not return an integer: " ++ show obj1
                 else return False -- XXX should this raise an exception?

dir :: Object -> Eval Object
dir object = do
   let maybeObjDict = dictOf object
   objectType <- typeOf object
   let objectBasesDicts = map dictOf $ getTupleElements $ object_mro $ objectType
   let allDicts = catMaybes (maybeObjDict : objectBasesDicts)
   let hashTables = map object_hashTable allDicts
   keyObjects <- concat <$> mapM keys hashTables
   let keyStrings = nub $ map (deMangle . object_string) keyObjects
   list $ map string keyStrings

hasAttribute :: Hashed String -> Object -> Eval Bool
hasAttribute ident object = isJust <$> lookupAttributeMaybe object ident

-- XXX not really correct. We should check that it has a method called "__iter__" rather
-- than just an attribute
isIterator :: Object -> Eval Bool
isIterator = hasAttribute specialIterName
