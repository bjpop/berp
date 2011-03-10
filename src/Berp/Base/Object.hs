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
import Berp.Base.Prims (callMethod, showObject)
import Data.List (nub)
import Control.Monad (zipWithM)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, catMaybes)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Mangle (deMangle)
import Berp.Base.Identity (Identity)
import Berp.Base.Hash (Hashed)
import Berp.Base.StdNames (specialEqName, specialCmpName, specialIterName)
import Berp.Base.LiftedIO as LIO (MonadIO)
#ifdef DEBUG
import Berp.Base.LiftedIO as LIO (putStrLn)
#endif
import {-# SOURCE #-} Berp.Base.HashTable (stringLookup, keys)
import {-# SOURCE #-} Berp.Base.StdTypes.Integer (intClass, int)
import {-# SOURCE #-} Berp.Base.StdTypes.Float (floatClass, float)
import {-# SOURCE #-} Berp.Base.StdTypes.Complex (complexClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (boolClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (tupleClass, getTupleElements)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (functionClass)
import {-# SOURCE #-} Berp.Base.StdTypes.String (stringClass, string)
import {-# SOURCE #-} Berp.Base.StdTypes.None (noneClass, noneIdentity)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (dictionaryClass)
import {-# SOURCE #-} Berp.Base.StdTypes.List (listClass, list)
import {-# SOURCE #-} Berp.Base.StdTypes.Generator (generatorClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Set (setClass)
import {-# SOURCE #-} Berp.Base.StdTypes.Module (moduleClass)

-- needed for overloaded numeric literals (integers)
instance Num Object where
   fromInteger = int
   (+) = undefined
   (*) = undefined
   abs = undefined
   signum = undefined

-- needed for overloaded numeric literals (floating point)
instance Fractional Object where
   fromRational x = float (fromRational x)
   (/) = undefined
   recip = undefined

-- Python allows the type of an object to change in a limited set of circumstances.
-- But we will ignore that for the moment and make it a pure function.
typeOf :: Object -> Object
typeOf obj@(Object {}) = object_type obj
typeOf obj@(Type {}) = object_type obj
typeOf (Integer {}) = intClass
typeOf (Float {}) = floatClass
typeOf (Complex {}) = complexClass
typeOf (Bool {}) = boolClass
typeOf (Tuple {}) = tupleClass
typeOf (List {}) = listClass
typeOf (Function {}) = functionClass
typeOf (String {}) = stringClass
typeOf (None {}) = noneClass
typeOf (Dictionary {}) = dictionaryClass
typeOf (Generator {}) = generatorClass
typeOf (Set {}) = setClass
typeOf (Module {}) = moduleClass

-- The identity of an object should never change so this can be a pure function.
identityOf :: Object -> Identity
identityOf None = noneIdentity
identityOf object = object_identity object

dictOf :: Object -> Maybe Object
dictOf obj@(Object {}) = Just $ object_dict obj
dictOf obj@(Type {}) = Just $ object_dict obj
dictOf obj@(Function {}) = Just $ object_dict obj
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

lookupSpecialAttributeMaybe :: MonadIO m => Object -> Hashed String -> m (Maybe Object)
lookupSpecialAttributeMaybe object ident = do
   BELCH("Looking for special attribute: " ++ show ident ++ " in: " ++ show object)
   lookupAttributeType object ident

lookupAttributeMaybe :: MonadIO m => Object -> Hashed String -> m (Maybe Object)
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

lookupAttributeType :: MonadIO m => Object -> Hashed String -> m (Maybe Object)
lookupAttributeType object ident = do
   BELCH("Looking in dict of the type: " ++ show objectType)
   let mroList = getTupleElements $ object_mro objectType
   searchMRO mroList
   where
   objectType :: Object
   objectType = typeOf object
   searchMRO :: MonadIO m => [Object] -> m (Maybe Object)
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
objectEquality obj1@(Bool {}) obj2@(Bool {})
   = return (object_bool obj1 == object_bool obj2)
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
           then truth <$> callMethod obj1 specialEqName [obj2]
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
   let objectBasesDicts = map dictOf $ getTupleElements $ object_mro $ typeOf object
   let allDicts = catMaybes (maybeObjDict : objectBasesDicts)
   let hashTables = map object_hashTable allDicts
   keyObjects <- concat <$> mapM keys hashTables
   let keyStrings = nub $ map (deMangle . object_string) keyObjects
   list $ map string keyStrings

hasAttribute :: (Functor m, MonadIO m) => Hashed String -> Object -> m Bool
hasAttribute ident object = isJust <$> lookupAttributeMaybe object ident

-- XXX not really correct. We should check that it has a method called "__iter__" rather
-- than just an attribute
isIterator :: Object -> Eval Bool
isIterator = hasAttribute specialIterName
