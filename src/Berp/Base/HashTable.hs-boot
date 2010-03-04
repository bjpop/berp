module Berp.Base.HashTable
   ( empty
   , insert
   , lookup
   , delete
   , hashObject
   , fromList
   , stringTableFromList
   , stringLookup
   ) where

import Prelude hiding (lookup)
import Berp.Base.Hash (Hashed)
import Berp.Base.SemanticTypes (Object, Eval, HashTable)

hashObject :: Object -> Eval Int
empty :: IO HashTable 
fromList :: [(Object, Object)] -> Eval HashTable
insert :: Object -> Object -> HashTable -> Eval ()
lookup :: Object -> HashTable -> Eval (Maybe Object)
delete :: Object -> HashTable -> Eval ()
stringTableFromList :: [(Hashed String, Object)] -> IO HashTable
stringLookup :: Hashed String -> HashTable -> IO (Maybe Object)
