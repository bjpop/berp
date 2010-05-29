{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.HashTable
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Mutable hashtable for the implementation of Python's dictionaries. 
--
-----------------------------------------------------------------------------

module Berp.Base.HashTable 
   ( empty
   , insert
   , lookup
   , delete
   , hashObject
   , fromList
   , stringTableFromList
   , stringLookup 
   , stringInsert
   , mappings
   , keys
   ) where

import Prelude hiding (lookup)
import qualified Data.IntMap as IntMap 
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Berp.Base.SemanticTypes (Object (..), Eval, HashTable)
import Berp.Base.Object (objectEquality)
import Berp.Base.Prims (callMethod)
import Berp.Base.Hash (hash, Hashed, hashedStr)
import Berp.Base.LiftedIO (MonadIO, readIORef, writeIORef, newIORef)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

mappings :: HashTable -> Eval [(Object, Object)]
mappings hashTable = do
   map <- readIORef hashTable
   return $ concat $ IntMap.elems map 

keys :: HashTable -> Eval [Object]
keys hashTable = map fst <$> mappings hashTable

hashObject :: Object -> Eval Int
hashObject obj@(String {}) = return $ hash $ object_string obj
hashObject obj@(Integer {}) = return $ hash $ object_integer obj
hashObject obj@(Bool {}) = if object_bool obj then return 1 else return 0
hashObject obj@(None {}) = return $ hash $ object_identity obj -- copying what Python3.0 seems to do
hashObject obj@(Function {}) = return $ hash $ object_identity obj
hashObject object = do
   hashResult <- callMethod object $(hashedStr "__hash__") []
   case hashResult of
      Integer {} -> return $ fromInteger $ object_integer hashResult
      _other -> fail $ "__hash__ method on object does not return an integer: " ++ show object

empty :: MonadIO m => m HashTable 
empty = newIORef IntMap.empty

fromList :: [(Object, Object)] -> Eval HashTable
fromList pairs = do
   keysVals <- mapM toKeyVal pairs
   newIORef $ IntMap.fromListWith (++) keysVals
   where
   toKeyVal :: (Object, Object) -> Eval (Int, [(Object, Object)])
   toKeyVal pair@(key, _val) = do
      hashValue <- hashObject key
      return (hashValue, [pair])

stringTableFromList :: MonadIO m => [(Hashed String, Object)] -> m HashTable
stringTableFromList pairs = do
   let keysVals = map toKeyVal pairs
   newIORef $ IntMap.fromListWith (++) keysVals
   where
   toKeyVal :: (Hashed String, Object) -> (Int, [(Object, Object)])
   toKeyVal ((hashValue,strKey), val) = 
      (hashValue, [(strObj, val)])
      where
      strObj = string strKey 

stringLookup :: MonadIO m => Hashed String -> HashTable -> m (Maybe Object)
stringLookup (hashValue, str) hashTable = do
   table <- readIORef hashTable
   case IntMap.lookup hashValue table of
      Nothing -> return Nothing
      Just matches -> return $ linearSearchString str matches
   where
   linearSearchString :: String -> [(Object, Object)] -> Maybe Object
   linearSearchString _ [] = Nothing
   linearSearchString str ((key, value) : rest)
      | objectEqualityString str key = Just value
      | otherwise = linearSearchString str rest

objectEqualityString :: String -> Object -> Bool
objectEqualityString str1 (String { object_string = str2 }) = str1 == str2
objectEqualityString _ _ = False

-- XXX Potential space leak by not deleteing old versions of key in the table.
-- maybe we can delete based on the identity of the object? That would not avoid
-- the leak in all cases, but it might work in common cases.
stringInsert :: Hashed String -> Object -> HashTable -> Eval ()
stringInsert (hashValue, s) value hashTable = do
   table <- readIORef hashTable
   -- hashValue <- hashObject key 
   let newTable = IntMap.insertWith (++) hashValue [(string s, value)] table
   writeIORef hashTable newTable 

-- XXX Potential space leak by not deleteing old versions of key in the table.
-- maybe we can delete based on the identity of the object? That would not avoid
-- the leak in all cases, but it might work in common cases.
insert :: Object -> Object -> HashTable -> Eval ()
insert key value hashTable = do
   table <- readIORef hashTable
   hashValue <- hashObject key 
   let newTable = IntMap.insertWith (++) hashValue [(key,value)] table
   writeIORef hashTable newTable 

lookup :: Object -> HashTable -> Eval (Maybe Object)
lookup key hashTable = do
   table <- readIORef hashTable
   hashValue <- hashObject key 
   case IntMap.lookup hashValue table of
      Nothing -> return Nothing
      Just matches -> linearSearch key matches 
   where
   linearSearch :: Object -> [(Object, Object)] -> Eval (Maybe Object)
   linearSearch _ [] = return Nothing
   linearSearch object ((key,value):rest) = do
      areEqual <- objectEquality object key
      if areEqual 
         then return (Just value)
         else linearSearch object rest 

linearFilter :: Object -> [(Object, Object)] -> Eval [(Object, Object)]
linearFilter object matches = foldM collectNotEquals [] matches 
   where
   collectNotEquals :: [(Object, Object)] -> (Object, Object) -> Eval [(Object, Object)]
   collectNotEquals acc pair@(key, _value) = do
      areEqual <- objectEquality object key
      return $ if areEqual then acc else pair:acc 

delete :: Object -> HashTable -> Eval ()
delete key hashTable = do
   table <- readIORef hashTable
   hashValue <- hashObject key
   case IntMap.lookup hashValue table of
      Nothing -> return ()
      Just matches -> do
         newMatches <- linearFilter key matches
         let newTable = IntMap.adjust (const newMatches) hashValue table
         writeIORef hashTable newTable
