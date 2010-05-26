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

import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Object (..), Procedure)
import Berp.Base.StdTypes.None (none)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDictionary)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

{-# NOINLINE function #-}
function :: Int -> Procedure -> Object 
function arity p = constantIO $ do 
   identity <- newIdentity
   dict <- emptyDictionary
   return $
      Function 
      { object_identity = identity 
      , object_dict = dict 
      , object_procedure = p
      , object_arity = arity
      }

{-# NOINLINE functionClass #-}
functionClass :: Object
functionClass = constantIO $ do 
   as <- attributes
   identity <- newIdentity 
   dict <- attributes
   newType [string "function", objectBase, dict]

-- XXX update my attributes
attributes :: IO Object 
attributes = mkAttributes []
