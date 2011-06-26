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
   , sizeIO
   ) where

import Control.Monad.Trans (liftIO)
import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import qualified Data.IntMap as IntMap
import Data.List (genericLength)
import Control.Monad (foldM)
import Berp.Base.SemanticTypes (Object (..), ObjectRef, Eval, HashTable)
import Berp.Base.Object (objectEquality)
import Berp.Base.Prims (callMethod)
import Berp.Base.Hash (hash, Hashed)
import Berp.Base.LiftedIO (MonadIO, readIORef, writeIORef, newIORef)
import Berp.Base.StdNames (specialHashName)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

mappings :: HashTable -> Eval [(Object, Object)]
mappings hashTable = do
   map <- readIORef hashTable
   let keysValsRefs = concat $ IntMap.elems map
   mapM readValRef keysValsRefs

readValRef :: (Object, ObjectRef) -> Eval (Object, Object)
readValRef (key, valRef) = do
   val <- readIORef valRef
   return (key, val)

keysIO :: HashTable -> IO [Object]
keysIO hashTable = do
   intMap <- readIORef hashTable
   let keysVals = concat $ IntMap.elems intMap
   return $ map fst keysVals

keys :: HashTable -> Eval [Object]
keys = liftIO . keysIO

sizeIO :: HashTable -> IO Integer
sizeIO hashTable = genericLength <$> keysIO hashTable

-- XXX This really belongs in another module, for example Hash.
hashObject :: Object -> Eval Int
hashObject obj@(String {}) = return $ hash $ object_string obj
hashObject obj@(Integer {}) = return $ hash $ object_integer obj
hashObject obj@(Bool {}) = if object_bool obj then return 1 else return 0
hashObject obj@(None {}) = return $ hash $ object_identity obj -- copying what Python3.0 seems to do
hashObject obj@(Function {}) = return $ hash $ object_identity obj
hashObject object = do
   hashResult <- callMethod object specialHashName []
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
   toKeyVal :: (Object, Object) -> Eval (Int, [(Object, ObjectRef)])
   toKeyVal (key, val) = do
      hashValue <- hashObject key
      valRef <- newIORef val
      return (hashValue, [(key,valRef)])

stringTableFromList :: MonadIO m => [(Hashed String, Object)] -> m HashTable
stringTableFromList pairs = do
   keysVals <- mapM toKeyVal pairs
   newIORef $ IntMap.fromListWith (++) keysVals
   where
   toKeyVal :: MonadIO m => (Hashed String, Object) -> m (Int, [(Object, ObjectRef)])
   toKeyVal ((hashValue,strKey), val) = do
      valRef <- newIORef val
      let strObj = string strKey 
      return (hashValue, [(strObj, valRef)])

stringLookup :: MonadIO m => Hashed String -> HashTable -> m (Maybe Object)
stringLookup (hashValue, str) hashTable = do
   table <- readIORef hashTable
   case IntMap.lookup hashValue table of
      Nothing -> return Nothing
      Just matches -> linearSearchString str matches
   where
   linearSearchString :: MonadIO m => String -> [(Object, ObjectRef)] -> m (Maybe Object)
   linearSearchString _ [] = return Nothing
   linearSearchString str ((key, valRef) : rest)
      | objectEqualityString str key = do
           val <- readIORef valRef
           return $ Just val
      | otherwise = linearSearchString str rest

objectEqualityString :: String -> Object -> Bool
objectEqualityString str1 (String { object_string = str2 }) = str1 == str2
objectEqualityString _ _ = False

stringInsert :: Hashed String -> Object -> HashTable -> Eval ()
stringInsert (hashValue, str) value hashTable = do
   table <- readIORef hashTable
   case IntMap.lookup hashValue table of
      Nothing -> do
         let stringObject = string str
         valRef <- newIORef value
         let newTable = IntMap.insert hashValue [(stringObject, valRef)] table 
         writeIORef hashTable newTable
      Just matches -> do
         updated <- linearInsertString str matches value
         if updated
            then return ()
            else do
               let stringObject = string str
               valRef <- newIORef value
               let newMatches = (stringObject, valRef) : matches
                   newTable = IntMap.insert hashValue newMatches table
               writeIORef hashTable newTable
   where
   linearInsertString :: MonadIO m => String -> [(Object, ObjectRef)] -> Object -> m Bool 
   linearInsertString _ [] _ = return False
   linearInsertString str ((key, valRef) : rest) obj
      | objectEqualityString str key = do
           writeIORef valRef obj
           return True
      | otherwise = linearInsertString str rest obj

-- XXX Potential space leak by not deleteing old versions of key in the table.
-- maybe we can delete based on the identity of the object? That would not avoid
-- the leak in all cases, but it might work in common cases.
{-
insert :: Object -> Object -> HashTable -> Eval ()
insert key value hashTable = do
   table <- readIORef hashTable
   hashValue <- hashObject key 
   let newTable = IntMap.insertWith (++) hashValue [(key,value)] table
   writeIORef hashTable newTable 
-}

insert :: Object -> Object -> HashTable -> Eval ()
insert key value hashTable = do
   table <- readIORef hashTable
   hashValue <- hashObject key 
   case IntMap.lookup hashValue table of
      Nothing -> do
         valRef <- newIORef value
         let newTable = IntMap.insert hashValue [(key, valRef)] table 
         writeIORef hashTable newTable 
      Just matches -> do
         updated <- linearInsert key matches value 
         if updated
            then return ()
            else do
               valRef <- newIORef value
               let newMatches = (key, valRef) : matches
                   newTable = IntMap.insert hashValue newMatches table
               writeIORef hashTable newTable
   where
   linearInsert :: Object -> [(Object, ObjectRef)] -> Object -> Eval Bool 
   linearInsert _ [] _ = return False 
   linearInsert probe ((key, valRef) : rest) obj = do
      areEqual <- objectEquality probe key
      if areEqual 
         then do
            writeIORef valRef obj
            return True 
         else linearInsert probe rest obj

lookup :: Object -> HashTable -> Eval (Maybe Object)
lookup key hashTable = do
   table <- readIORef hashTable
   hashValue <- hashObject key 
   case IntMap.lookup hashValue table of
      Nothing -> return Nothing
      Just matches -> linearSearch key matches 
   where
   linearSearch :: Object -> [(Object, ObjectRef)] -> Eval (Maybe Object)
   linearSearch _ [] = return Nothing
   linearSearch object ((key,valRef):rest) = do
      areEqual <- objectEquality object key
      if areEqual 
         then do
            val <- readIORef valRef
            return $ Just val
         else linearSearch object rest 

linearFilter :: Object -> [(Object, ObjectRef)] -> Eval [(Object, ObjectRef)]
linearFilter object matches = foldM collectNotEquals [] matches 
   where
   collectNotEquals :: [(Object, ObjectRef)] -> (Object, ObjectRef) -> Eval [(Object, ObjectRef)]
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
