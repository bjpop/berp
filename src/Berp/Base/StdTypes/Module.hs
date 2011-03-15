-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Module
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Python modules.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Module (mkModule, moduleClass) where

-- import Berp.Base.Prims (primitive)
import Control.Applicative ((<$>))
import Berp.Base.LiftedIO (readIORef)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Object (..), ObjectRef, Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Hash (Hashed)
-- import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

{-# NOINLINE moduleClass #-}
moduleClass :: Object
moduleClass = constantIO $ do
   dict <- attributes
   newType [string "module", objectBase, dict]

attributes :: IO Object
attributes = mkAttributes [ ]

-- Maybe this belongs in Prims, but we end up with more cyclic dependencies.
mkModule :: [(Hashed String, ObjectRef)] -> Eval Object
mkModule namesRefs = do
   namesObjs <- mapM toNameObj namesRefs
   dict <- mkAttributes namesObjs
   identity <- newIdentity
   return $
      Module { object_identity = identity
             , object_dict = dict }
   where
   toNameObj :: (Hashed String, ObjectRef) -> Eval (Hashed String, Object)
   toNameObj (s, ref) = ((,) s) <$> readIORef ref
