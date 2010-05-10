module Berp.Base.Attributes (mkAttributes) where

import Berp.Base.SemanticTypes (Object (..))
import Berp.Base.HashTable (stringTableFromList)
import Berp.Base.Hash (Hashed)
import Berp.Base.Identity (newIdentity)
import Berp.Base.LiftedIO (MonadIO)

mkAttributes :: MonadIO m => [(Hashed String, Object)] -> m Object
mkAttributes list = do
   hashTable <- stringTableFromList list 
   identity <- newIdentity
   return $ 
      Dictionary  
      { object_identity = identity
      , object_hashTable = hashTable 
      }
