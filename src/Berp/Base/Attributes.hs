module Berp.Base.Attributes (mkAttributes) where

import Berp.Base.SemanticTypes (Object (..))
import Berp.Base.HashTable (stringTableFromList)
import Berp.Base.Hash (Hashed)
import Berp.Base.Identity (newIdentity)

mkAttributes :: [(Hashed String, Object)] -> IO Object
mkAttributes list = do
   hashTable <- stringTableFromList list 
   identity <- newIdentity
   return $ 
      Dictionary  
      { object_identity = identity
      , object_hashTable = hashTable 
      }
