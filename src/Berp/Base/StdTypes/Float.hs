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

import Berp.Base.Monad (constantIO)
import Berp.Base.Prims (primitive, raise)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import Berp.Base.Builtins (notImplementedError)
import Berp.Base.Operators
   ( addFloatFloatFloat, addFloatIntFloat, subFloatFloatFloat, subFloatIntFloat,
     mulFloatFloatFloat, mulFloatIntFloat, ltFloatFloatBool, ltFloatIntBool,
     leFloatFloatBool, leFloatIntBool, gtFloatFloatBool, gtFloatIntBool,
     geFloatFloatBool, geFloatIntBool, eqFloatFloatBool, eqFloatIntBool,
     divFloatFloatFloat, divFloatIntFloat)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

{-# NOINLINE float #-}
float :: Double -> Object
float f = constantIO $ do
   identity <- newIdentity
   return $ Float { object_identity = identity, object_float = f }

{-# NOINLINE floatClass #-}
floatClass :: Object
floatClass = constantIO $ do
   dict <- attributes
   newType [string "float", objectBase, dict]

attributes :: IO Object
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

mkOp :: (Object -> Object -> Eval Object) -> (Object -> Object -> Eval Object) -> Object
mkOp opFloat opInt = primitive 2 $ \[x,y] ->
   case y of
      Float {} -> opFloat x y
      Integer {} -> opInt x y
      _other -> raise notImplementedError

add :: Object
add = mkOp addFloatFloatFloat addFloatIntFloat

sub :: Object
sub = mkOp subFloatFloatFloat subFloatIntFloat

mul :: Object
mul = mkOp mulFloatFloatFloat mulFloatIntFloat

divide :: Object
divide = mkOp divFloatFloatFloat divFloatIntFloat

lt :: Object
lt = mkOp ltFloatFloatBool ltFloatIntBool

le :: Object
le = mkOp leFloatFloatBool leFloatIntBool

gt :: Object
gt = mkOp gtFloatFloatBool gtFloatIntBool

ge :: Object
ge = mkOp geFloatFloatBool geFloatIntBool

eq :: Object
eq = mkOp eqFloatFloatBool eqFloatIntBool

str :: Object
str = primitive 1 $ \[x] -> return $ string $ show $ object_float x
