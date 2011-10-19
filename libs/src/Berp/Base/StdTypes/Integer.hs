-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Integer
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard integer type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Integer (int, intClass) where

import Berp.Base.Prims (primitive, raiseException)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Attributes (mkAttributesList)
-- import Berp.Base.Builtins (notImplementedError)
import Berp.Base.StdNames
import Berp.Base.Operators
   ( addIntIntInt, subIntIntInt, modIntIntInt, mulIntIntInt, ltIntIntBool
   , leIntIntBool, gtIntIntBool, geIntIntBool, eqIntIntBool, divIntIntInt)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

int :: Integer -> Object
int i = Integer { object_integer = i }

intClass :: Eval Object
intClass = do
   dict <- attributes
   base <- objectBase
   newType [string "int", base, dict]

attributes :: Eval Object
attributes = mkAttributesList
   [ (specialAddName, add)
   , (specialSubName, sub)
   , (specialMulName, mul)
   , (specialDivName, divide)
   , (specialLtName, lt)
   , (specialLeName, le)
   , (specialGtName, gt)
   , (specialGeName, ge)
   , (specialEqName, eq)
   , (specialStrName, str)
   , (specialModName, modulus)
   ]

mkOp :: (Object -> Object -> Eval Object) -> Eval Object
mkOp op = primitive 2 fun
   where
   fun (x:y:_) =
      case y of
         Integer {} -> op x y
         _other -> raiseException "notImplementedError"
   fun _other = error "operator on Int applied to wrong number of arguments"

add :: Eval Object
add = mkOp addIntIntInt

sub :: Eval Object
sub = mkOp subIntIntInt

mul :: Eval Object
mul = mkOp mulIntIntInt

divide :: Eval Object
divide = mkOp divIntIntInt

lt :: Eval Object
lt = mkOp ltIntIntBool

le :: Eval Object
le = mkOp leIntIntBool

gt :: Eval Object
gt = mkOp gtIntIntBool

ge :: Eval Object
ge = mkOp geIntIntBool

eq :: Eval Object
eq = mkOp eqIntIntBool

modulus :: Eval Object
modulus = mkOp modIntIntInt

str :: Eval Object
str = primitive 1 fun
   where
   fun (x:_) = return $ string $ show $ object_integer x
   fun _other = error "str method on Int applied to the wrong number of arguments"
