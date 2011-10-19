-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Function
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard function type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Function (function, functionClass) where

import Berp.Base.SemanticTypes (Object (..), Procedure, Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdTypes.Dictionary (emptyDictionary)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

function :: Int -> Procedure -> Eval Object
function arity proc = do
   identity <- newIdentity
   dict <- emptyDictionary
   return $
      Function
      { object_identity = identity
      , object_dict = dict
      , object_procedure = proc
      , object_arity = arity
      }

functionClass :: Eval Object
functionClass = do
   dict <- attributes
   base <- objectBase
   newType [string "function", base, dict]

-- XXX update my attributes
attributes :: Eval Object
attributes = mkAttributesList []
