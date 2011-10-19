-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Truth
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Implementation of the truth predicate on Python objects.
--
-----------------------------------------------------------------------------

module Berp.Base.Truth (truth) where

import {-# SOURCE #-} Berp.Base.HashTable as HashTable (size)
import {-# SOURCE #-} Berp.Base.HashSet as HashSet (size)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Data.Complex (Complex (..))
import Berp.Base.LiftedIO (readIORef)

truth :: Object -> Eval Bool
-- truth (Bool { object_bool = b }) = b
truth TrueObject = return True
truth FalseObject = return False
truth (Integer { object_integer = i }) = return (i /= 0)
truth (Float { object_float = f }) = return (f /= 0)
truth (Complex { object_complex = c }) = return (c /= (0 :+ 0))
truth None = return False
truth obj@(Tuple {}) = return (object_length obj /= 0)
truth obj@(String {}) = return (object_string obj /= [])
truth obj@(List {}) = do
   numElems <- readIORef $ object_list_num_elements obj
   return (numElems /= 0)
truth obj@(Dictionary {}) = do
   numElems <- HashTable.size $ object_hashTable obj
   return (numElems /= 0)
truth obj@(Set {}) = do
   numElems <- HashSet.size $ object_hashSet obj
   return (numElems /= 0)
truth _other = return True

{-
   Python Language Reference, section 5.10 "Boolean operations":

   In the context of Boolean operations, and also when expressions 
   are used by control flow statements, the following values are 
   interpreted as false: False, None, numeric zero of all types, 
   and empty strings and containers (including strings, tuples, 
   lists, dictionaries, sets and frozensets). All other values 
   are interpreted as true. User-defined objects can customize their 
   truth value by providing a __bool__() method.
-}
