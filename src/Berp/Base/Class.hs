-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}
-- uncomment one of the two above lines to turn debugging on/off for this module

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Class
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Implementation of the Python "class" keyword. We call it "klass" (with a k) 
-- because "class" is a keyword in Haskell.
--
-----------------------------------------------------------------------------

#include "BerpDebug.h"

module Berp.Base.Class (klass) where

import Berp.Base.LiftedIO (liftIO, MonadIO, writeIORef, readIORef, newIORef)
import Data.Map (fromList)
import Berp.Base.Ident
import Berp.Base.SemanticTypes (Eval, Procedure, Object (..), ObjectRef)
import Berp.Base.Prims (printObject)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Dictionary (emptyDictionary)
import Berp.Base.StdTypes.Tuple (emptyTuple, tuple)
import Berp.Base.StdTypes.None (none)
import Berp.Base.StdTypes.Object (object)

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
