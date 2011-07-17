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
   ( importModule, importAll, run ) where

import Control.Monad (foldM_)
import Control.Monad.State (get, put)
import Berp.Base.SemanticTypes (HashTable, Eval, Object (..), initState, EvalState (..))
import Berp.Base.Monad (lookupModuleCache, updateModuleCache, runEval)
import Berp.Base.HashTable as HashTable (empty, mappings, insert)
import Berp.Base.Builtins (initBuiltins)
import Berp.Base.StdTypes.None (none)
import Berp.Base.Prims (printObject, getGlobalScopeHashTable)
import Berp.Base.LiftedIO as LIO (putStr, putStrLn)

importModule :: FilePath -> Eval Object -> Eval Object
importModule path comp = do
   maybeImported <- lookupModuleCache path
   case maybeImported of
      Just obj -> return obj
      Nothing -> do
         beforeModuleState <- get
         emptyTable <- HashTable.empty
         let newState = initState emptyTable
         put $ newState { state_moduleCache = state_moduleCache beforeModuleState }
         obj <- initBuiltins >> comp
         afterModuleState <- get
         put $ beforeModuleState { state_moduleCache = state_moduleCache afterModuleState }
         updateModuleCache path obj
         return obj

run :: Eval Object -> Prelude.IO ()
run comp = do
   table <- HashTable.empty
   _ <- runEval (initState table) (initBuiltins >> comp)
   return ()

importAll :: Object -> Eval Object
importAll obj =
    case obj of
       Module { object_dict = dict } ->
          case dict of
             Dictionary { object_hashTable = hashTable } -> do
                globalScopeHashTable <- getGlobalScopeHashTable
                items <- HashTable.mappings hashTable
                foldM_ updateTable globalScopeHashTable items
                globalScopeHashTable <- getGlobalScopeHashTable
                items <- HashTable.mappings globalScopeHashTable
                -- LIO.putStrLn "In importAll"
                -- printItems items
                return none
             _other -> return none
       -- XXX maybe this should be an error
       _other -> return none
   where
   updateTable :: HashTable -> (Object, Object) -> Eval HashTable
   updateTable ht (key, val) = do
      HashTable.insert key val ht
      return ht

printItems :: [(Object, Object)] -> Eval ()
printItems = mapM_ (\(k,v) -> do { printObject k; LIO.putStr " "; printObject v; LIO.putStr "\n" })
