-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Float
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard floating point type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Float (float, floatClass) where

import Berp.Base.Prims (primitive, raiseException)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import Berp.Base.Operators
   ( addFloatFloatFloat, addFloatIntFloat, subFloatFloatFloat, subFloatIntFloat,
     mulFloatFloatFloat, mulFloatIntFloat, ltFloatFloatBool, ltFloatIntBool,
     leFloatFloatBool, leFloatIntBool, gtFloatFloatBool, gtFloatIntBool,
     geFloatFloatBool, geFloatIntBool, eqFloatFloatBool, eqFloatIntBool,
     divFloatFloatFloat, divFloatIntFloat)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

float :: Double -> Object
float f = Float { object_float = f }

floatClass :: Eval Object
floatClass = do
   dict <- attributes
   base <- objectBase
   newType [string "float", base, dict]

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
   ]

mkOp :: (Object -> Object -> Eval Object) -> (Object -> Object -> Eval Object) -> Eval Object
mkOp opFloat opInt = primitive 2 fun
   where
   fun (x:y:_) =
      case y of
         Float {} -> opFloat x y
         Integer {} -> opInt x y
         _other -> raiseException "notImplementedError"
   fun _other = error "operator on Float applied to wrong number of arguments"

add :: Eval Object
add = mkOp addFloatFloatFloat addFloatIntFloat

sub :: Eval Object
sub = mkOp subFloatFloatFloat subFloatIntFloat

mul :: Eval Object
mul = mkOp mulFloatFloatFloat mulFloatIntFloat

divide :: Eval Object
divide = mkOp divFloatFloatFloat divFloatIntFloat

lt :: Eval Object
lt = mkOp ltFloatFloatBool ltFloatIntBool

le :: Eval Object
le = mkOp leFloatFloatBool leFloatIntBool

gt :: Eval Object
gt = mkOp gtFloatFloatBool gtFloatIntBool

ge :: Eval Object
ge = mkOp geFloatFloatBool geFloatIntBool

eq :: Eval Object
eq = mkOp eqFloatFloatBool eqFloatIntBool

str :: Eval Object
str = primitive 1 fun
   where
   fun (x:_) = return $ string $ show $ object_float x
   fun _other = error "str method on Float applied to wrong number of arguments"
