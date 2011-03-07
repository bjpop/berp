-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.HashSet
-- Copyright   : (c) 2011 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Mutable hashset for the implementation of Python's sets.
--
-----------------------------------------------------------------------------

module Berp.Base.HashSet
   ( empty
   , insert
   , lookup
   , delete
   , fromList
   , elements
   , sizeIO
   ) where

import Control.Monad.Trans (liftIO)
import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import qualified Data.IntMap as IntMap
import Data.List (genericLength)
import Control.Monad (foldM)
import Berp.Base.SemanticTypes (Object (..), Eval, HashSet)
import Berp.Base.Object (objectEquality)
import Berp.Base.HashTable (hashObject)
import Berp.Base.LiftedIO (MonadIO, readIORef, writeIORef, newIORef)

elementsIO :: HashSet -> IO [Object]
elementsIO hashSet = concat <$> IntMap.elems <$> readIORef hashSet

elements :: HashSet -> Eval [Object]
elements = liftIO . elementsIO

sizeIO :: HashSet -> IO Integer
sizeIO hashSet = genericLength <$> elementsIO hashSet

empty :: MonadIO m => m HashSet
empty = newIORef IntMap.empty

fromList :: [Object] -> Eval HashSet
fromList objs = do
   elementsVals <- mapM toElement objs
   newIORef $ IntMap.fromListWith (++) elementsVals
   where
   toElement :: Object -> Eval (Int, [Object])
   toElement obj = do
      hashValue <- hashObject obj
      return (hashValue, [obj])

insert :: Object -> HashSet -> Eval ()
insert element hashSet = do
   table <- readIORef hashSet
   hashValue <- hashObject element
   case IntMap.lookup hashValue table of
      Nothing -> do
         let newTable = IntMap.insert hashValue [element] table
         writeIORef hashSet newTable
      Just matches -> do
         newMatches <- linearInsert element matches
         let newTable = IntMap.insert hashValue newMatches table
         writeIORef hashSet newTable
   where
   linearInsert :: Object -> [Object] -> Eval [Object]
   linearInsert obj [] = return [obj]
   linearInsert obj1 list@(obj2:rest) = do
      areEqual <- objectEquality obj1 obj2
      if areEqual
         then return list
         else (obj2 :) <$> linearInsert obj1 rest

lookup :: Object -> HashSet -> Eval Bool
lookup element hashSet = do
   table <- readIORef hashSet
   hashValue <- hashObject element
   case IntMap.lookup hashValue table of
      Nothing -> return False
      Just matches -> linearSearch element matches
   where
   linearSearch :: Object -> [Object] -> Eval Bool
   linearSearch _ [] = return False
   linearSearch obj1 (obj2:rest) = do
      areEqual <- objectEquality obj1 obj2
      if areEqual
         then return True
         else linearSearch obj1 rest

linearFilter :: Object -> [Object] -> Eval [Object]
linearFilter object matches = foldM collectNotEquals [] matches
   where
   collectNotEquals :: [Object] -> Object -> Eval [Object]
   collectNotEquals acc next = do
      areEqual <- objectEquality object next
      return $ if areEqual then acc else object:acc

delete :: Object -> HashSet -> Eval ()
delete element hashSet = do
   table <- readIORef hashSet
   hashValue <- hashObject element
   case IntMap.lookup hashValue table of
      Nothing -> return ()
      Just matches -> do
         newMatches <- linearFilter element matches
         let newTable = IntMap.adjust (const newMatches) hashValue table
         writeIORef hashSet newTable
