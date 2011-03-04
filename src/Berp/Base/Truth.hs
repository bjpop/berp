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

import {-# SOURCE #-} Berp.Base.HashTable as HashTable (sizeIO)
import Berp.Base.SemanticTypes (Object (..))
import Data.Complex (Complex (..))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (readIORef)

-- XXX incomplete
truth :: Object -> Bool
truth (Bool { object_bool = b }) = b
truth (Integer { object_integer = i }) = i /= 0
truth (Float { object_float = f }) = f /= 0
truth (Complex { object_complex = c }) = c /= (0 :+ 0)
truth None = False
truth obj@(Tuple {}) = object_length obj /= 0
truth obj@(String {}) = object_string obj /= []
truth obj@(List {}) = unsafePerformIO $ do
   numElems <- readIORef $ object_list_num_elements obj
   return (numElems /= 0)
truth obj@(Dictionary {}) = unsafePerformIO $ do
   numElems <- HashTable.sizeIO $ object_hashTable obj
   return (numElems /= 0)
truth _other = True

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
