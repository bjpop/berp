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

module Berp.Base.StdTypes.None (none, noneClass) where

import Berp.Base.Prims (primitive)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval)
import Berp.Base.StdTypes.Bool (true, false)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

none :: Object
none = None

noneClass :: Eval Object
noneClass = do
   dict <- attributes
   base <- objectBase
   newType [string "NoneType", base, dict]

attributes :: Eval Object
attributes = mkAttributesList
   [ (specialEqName, primitive 2 eq)
   , (specialStrName, primitive 1 str)
   ]

eq :: Procedure
eq [None, None] = return true
eq _ = Prelude.return false

str :: Procedure 
str _ = Prelude.return $ string "None"
