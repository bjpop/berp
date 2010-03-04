{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.Class (klass) where

import Data.IORef (writeIORef, newIORef, readIORef)
import Control.Monad.Trans (liftIO)
import Data.Map (fromList)
import Berp.Base.Ident
-- import Berp.Base.Monad (Eval)
import Berp.Base.SemanticTypes (Eval, Procedure, VarEnv, Object (..), ObjectRef)
import Berp.Base.Prims ((@@))
import Berp.Base.Identity (newIdentity)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.Attributes (mkAttributes)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDict)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (tuple)

klass :: Ident -> ObjectRef -> [ObjectRef] -> Eval [(Hashed String, ObjectRef)] -> Eval ()
klass className ident basesRefs attributesComp = do
   attributes <- attributesComp 
   attributesObjects <- liftIO $ mapM getIdentObj attributes
   classDict <- liftIO $ mkAttributes attributesObjects
   -- The cycle in the definition is okay despite the object_procedure field
   -- being strict (see the definition of Object). It is okay because the
   -- Procedure type is a function (functions are values in Haskell).
   bases <- liftIO $ mapM readIORef basesRefs 
   identity <- liftIO $ newIdentity
   let typeObject = 
          Type 
          { object_identity = identity
          , object_type = typeClass
          , object_dict = classDict 
          , object_bases = tuple bases 
          , object_constructor = mkProcedure typeObject 
          , object_type_name = string className
          }
   liftIO $ writeIORef ident $ typeObject 
   where
   mkProcedure :: Object -> Procedure
   mkProcedure typeObject args = liftIO $ do
      identity <- newIdentity
      dict <- emptyDict
      return $
         Object
         { object_identity = identity
         , object_type = typeObject 
         , object_dict = dict
         }
   getIdentObj :: (a, ObjectRef) -> IO (a, Object) 
   getIdentObj (ident, ref) = do
      obj <- readIORef ref
      return (ident, obj)

init_name :: Hashed String
init_name = $(hashedStr "__init__")
