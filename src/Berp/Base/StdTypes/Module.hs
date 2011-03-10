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

module Berp.Base.StdTypes.Module (moduleClass) where

-- import Berp.Base.Prims (primitive)
import Berp.Base.Monad (constantIO)
-- import Berp.Base.SemanticTypes (Procedure, Object (..))
import Berp.Base.SemanticTypes (Object (..))
-- import Berp.Base.Identity (newIdentity, Identity)
import Berp.Base.Attributes (mkAttributes)
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
