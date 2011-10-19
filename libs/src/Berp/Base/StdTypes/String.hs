-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.String
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard string type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.String (string, stringClass, emptyString) where

import Berp.Base.Prims (primitive)
import Berp.Base.Prims (binOp)
import Berp.Base.SemanticTypes (Object (..), Eval)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (bool)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)

emptyString :: Object
emptyString = string ""

string :: String -> Object
string str = String { object_string = str }

stringClass :: Eval Object
stringClass = do
   dict <- attributes
   base <- objectBase
   newType [string "str", base, dict]

attributes :: Eval Object
attributes = mkAttributesList
   [ (specialEqName, eq)
   , (specialStrName, str)
   , (specialAddName, add)
   ]

eq :: Eval Object
eq = primitive 2 fun
   where
   fun (x:y:_) = binOp x y object_string (==) (Prelude.return . bool)
   fun _other = error "eq method on strings applied to wrong number of arguments"

str :: Eval Object
str = primitive 1 fun
   where
   fun (x:_) = Prelude.return x
   fun _other = error "str method on strings applied to wrong number of arguments"

add :: Eval Object
add = primitive 2 fun
   where
   fun (x:y:_) = binOp x y object_string (++) (Prelude.return . string)
   fun _other = error "add method on strings applied to wrong number of arguments"
