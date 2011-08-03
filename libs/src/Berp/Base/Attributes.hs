-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Attributes
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Build a dictionary of the attributes of an object from a list of 
-- pairs containing the hashed name of the attribute and the corresponding 
-- value.
--
-----------------------------------------------------------------------------

module Berp.Base.Attributes (mkAttributes, mkAttributesList) where

import Berp.Base.SemanticTypes (HashTable, Object (..))
import Berp.Base.HashTable (stringTableFromList)
import Berp.Base.Hash (Hashed)
import Berp.Base.Identity (newIdentity)
import Berp.Base.LiftedIO (MonadIO)

mkAttributesList :: MonadIO m => [(Hashed String, Object)] -> m Object
mkAttributesList list =
   -- hashTable <- stringTableFromList list
   mkAttributes =<< stringTableFromList list
{-
   identity <- newIdentity
   return $
      Dictionary
      { object_identity = identity
      , object_hashTable = hashTable
      }
-}

mkAttributes :: MonadIO m => HashTable -> m Object
mkAttributes hashTable = do
   identity <- newIdentity
   return $
      Dictionary
      { object_identity = identity
      , object_hashTable = hashTable
      }
