-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Dictionary
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard dictionary type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Dictionary (emptyDictionary, dictionary, dictionaryClass) where

import Data.List (intersperse)
import Berp.Base.Prims (primitive, callMethod, showObject)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval)
import Berp.Base.StdTypes.String (string)
import Berp.Base.Identity (newIdentity)
import Berp.Base.HashTable as Hash (fromList, empty, mappings, lookup)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

emptyDictionary :: IO Object
emptyDictionary = do 
   identity <- newIdentity
   hashTable <- Hash.empty 
   return $ 
      Dictionary 
      { object_identity = identity
      , object_hashTable = hashTable 
      }

dictionary :: [(Object, Object)] -> Eval Object
dictionary elements = do 
   identity <- newIdentity
   hashTable <- fromList elements
   return $ 
      Dictionary 
      { object_identity = identity
      , object_hashTable = hashTable 
      }

{-# NOINLINE dictionaryClass #-}
dictionaryClass :: Object
dictionaryClass = constantIO $ do 
   identity <- newIdentity
   dict <- attributes
   newType [string "dict", objectBase, dict]

attributes :: IO Object 
attributes = mkAttributes 
   [ (eqName, primitive 2 eq)
   , (strName, primitive 1 str)
   , (getItemName, primitive 2 getItem)
   ]

eq :: Procedure 
eq = error "== on dict not defined"

str :: Procedure 
str (obj:_) = do
   ms <- mappings $ object_hashTable obj
   strs <- mapM dictEntryString ms
   return $ string ("{" ++ concat (intersperse ", " strs) ++ "}")
   where
   dictEntryString :: (Object, Object) -> Eval String
   dictEntryString (obj1, obj2) = do
      objStr1 <- showObject obj1
      objStr2 <- showObject obj2
      return (objStr1 ++ ": " ++ objStr2)

getItem :: Procedure
getItem (obj:index:_) = do
   let ht = object_hashTable obj
   maybeVal <- Hash.lookup index ht
   case maybeVal of 
      Nothing -> error "dict lookup failed" 
      Just val -> return val  
