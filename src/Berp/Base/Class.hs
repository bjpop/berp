-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}
-- uncomment one of the two above lines to turn debugging on/off for this module
#include "BerpDebug.h"

module Berp.Base.Class (klass) where

-- import Data.IORef (writeIORef, newIORef, readIORef)

import Berp.Base.LiftedIO (liftIO, MonadIO, writeIORef, readIORef, newIORef)
import Data.Map (fromList)
import Berp.Base.Ident
import Berp.Base.SemanticTypes (Eval, Procedure, Object (..), ObjectRef)
import Berp.Base.Prims (printObject)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.Attributes (mkAttributes)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDictionary)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (emptyTuple, tuple)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.Object (object)

klass :: Ident -> ObjectRef -> [Object] -> Eval [(Hashed String, ObjectRef)] -> Eval Object 
klass className ident srcBases attributesComp = do
   -- if the source lists no bases for the class, then force it to be (object)
   let trueBases = if null srcBases then [object] else srcBases 
   attributes <- attributesComp 
   attributesObjects <- mapM getIdentObj attributes
   classDict <- mkAttributes attributesObjects
   typeObject <- liftIO $ newType [string className, tuple trueBases, classDict]
   writeIORef ident $ typeObject 
   IF_DEBUG((printObject $ object_mro typeObject) >> putStr "\n")
   return none
   where
   getIdentObj :: MonadIO m => (a, ObjectRef) -> m (a, Object) 
   getIdentObj (ident, ref) = do
      obj <- readIORef ref
      return (ident, obj)
