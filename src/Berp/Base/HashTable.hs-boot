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
   , keys
   ) where

import Prelude hiding (lookup)
import Berp.Base.Hash (Hashed)
import Berp.Base.SemanticTypes (Object, Eval, HashTable)
import Berp.Base.LiftedIO (MonadIO)

hashObject :: Object -> Eval Int
empty :: MonadIO m => m HashTable 
fromList :: [(Object, Object)] -> Eval HashTable
insert :: Object -> Object -> HashTable -> Eval ()
lookup :: Object -> HashTable -> Eval (Maybe Object)
delete :: Object -> HashTable -> Eval ()
stringTableFromList :: MonadIO m => [(Hashed String, Object)] -> m HashTable
stringLookup :: MonadIO m => Hashed String -> HashTable -> m (Maybe Object)
stringInsert :: Hashed String -> Object -> HashTable -> Eval ()
keys :: HashTable -> Eval [Object]
