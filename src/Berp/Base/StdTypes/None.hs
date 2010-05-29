-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.None
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard none type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.None (none, noneIdentity, noneClass) where

import Berp.Base.Prims (primitive)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Procedure, Object (..))
import Berp.Base.StdTypes.Bool (true, false)
import Berp.Base.Identity (newIdentity, Identity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

none :: Object 
none = None 

{-# NOINLINE noneIdentity #-}
noneIdentity :: Identity
noneIdentity = constantIO newIdentity

{-# NOINLINE noneClass #-}
noneClass :: Object
noneClass = constantIO $ do 
   identity <- newIdentity
   dict <- attributes
   newType [string "NoneType", objectBase, dict]

attributes :: IO Object 
attributes = mkAttributes 
   [ (eqName, primitive 2 eq)
   , (strName, primitive 1 str)
   ]
        
eq :: Procedure 
eq [None, None] = return true 
eq _ = Prelude.return false 

str :: Procedure 
str [x] = Prelude.return $ string "None" 
