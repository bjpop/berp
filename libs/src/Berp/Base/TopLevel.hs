-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.TopLevel
-- Copyright   : (c) 2011 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Functions to execute the top-level computations.
--
-----------------------------------------------------------------------------

module Berp.Base.TopLevel
   ( importModule, importAll, run, runWithGlobals ) where

import Control.Concurrent.MVar (newMVar)
import Control.Monad (foldM_)
import Berp.Base.SemanticTypes (HashTable, Eval, Object (..), initState)
import Berp.Base.Monad (lookupModuleCache, updateModuleCache, runEval)
import Berp.Base.HashTable as HashTable (empty, mappings, insert)
import Berp.Base.Builtins (initBuiltins)
import Berp.Base.LiftedIO as LIO (putStrLn)
import Berp.Base.StdTypes.Module (mkModule)
import Berp.Base.Unique (zero)

importModule :: FilePath -> (HashTable -> Eval Object) -> Eval Object
importModule path comp = do
   maybeImported <- lookupModuleCache path
   case maybeImported of
      Just obj -> return obj
      Nothing -> do
         -- XXX fixme
         -- beforeModuleState <- get
         moduleScope <- HashTable.empty
         -- let newState = initState
         -- put $ newState { state_moduleCache = state_moduleCache beforeModuleState }
         -- obj <- initBuiltins emptyTable >> comp
         _ <- comp moduleScope
         -- afterModuleState <- get
         -- put $ beforeModuleState { state_moduleCache = state_moduleCache afterModuleState }
         moduleObj <- mkModule moduleScope
         updateModuleCache path moduleObj
         return moduleObj

run :: (HashTable -> Eval Object) -> Prelude.IO ()
run comp = do
   globals <- HashTable.empty
   runWithGlobals globals comp

{-
   builtins <- HashTable.empty
   _ <- runEval (initState builtins) (initBuiltins builtins >> comp globalScope)
   return ()
-}

runWithGlobals :: HashTable -> (HashTable -> Eval Object) -> Prelude.IO ()
runWithGlobals globals comp = do
   builtins <- HashTable.empty
   LIO.putStrLn "before unique"
   initUnique <- newMVar zero
   LIO.putStrLn "after unique"
   _ <- runEval (initState initUnique builtins) $ do
           LIO.putStrLn "before initBuiltins"
           initBuiltins builtins
           LIO.putStrLn "after initBuiltins"
           x <- comp globals
           LIO.putStrLn "after comp"
           return x
   LIO.putStrLn "after eval"
   return ()


importAll :: HashTable -> Object -> Eval ()
importAll globalScope obj =
    case obj of
       Module { object_dict = dict } ->
          case dict of
             Dictionary { object_hashTable = hashTable } -> do
                -- globalScopeHashTable <- getGlobalScopeHashTable
                items <- HashTable.mappings hashTable
                foldM_ updateTable globalScope items
                -- globalScopeHashTable <- getGlobalScopeHashTable
                -- items <- HashTable.mappings globalScopeHashTable
                -- LIO.putStrLn "In importAll"
                return ()
             _other -> return ()
       -- XXX maybe this should be an error
       _other -> return ()
   where
   updateTable :: HashTable -> (Object, Object) -> Eval HashTable
   updateTable ht (key, val) = do
      HashTable.insert key val ht
      return ht
