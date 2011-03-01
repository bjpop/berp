-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Complex
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard floating point type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Complex (complex, complexClass) where

import Data.Complex (Complex (..), realPart, imagPart)
import Berp.Base.Monad (constantIO)
import Berp.Base.Prims (binOp, primitive, raise)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.StdTypes.Bool (bool)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import Berp.Base.Builtins (notImplementedError)
import Berp.Base.Operators
   ( addComplexComplexComplex
   , addComplexIntComplex
   , addComplexFloatComplex
   , subComplexComplexComplex
   , subComplexIntComplex
   , subComplexFloatComplex
   , mulComplexComplexComplex
   , mulComplexIntComplex
   , mulComplexFloatComplex
   , divComplexComplexComplex
   , divComplexIntComplex
   , divComplexFloatComplex
   , eqComplexComplexBool
   , eqComplexIntBool
   , eqComplexFloatBool )
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

{-# NOINLINE complex #-}
complex :: Complex Double -> Object
complex c = constantIO $ do
   identity <- newIdentity
   return $ Complex { object_identity = identity, object_complex = c }

{-# NOINLINE complexClass #-}
complexClass :: Object
complexClass = constantIO $ do
   dict <- attributes
   newType [string "complex", objectBase, dict]

attributes :: IO Object
attributes = mkAttributes
   [ (addName, add)
   , (subName, sub)
   , (mulName, mul)
   , (divName, divide)
   , (eqName, eq)
   , (strName, str)
   ]

mkOp :: (Object -> Object -> Eval Object) ->
        (Object -> Object -> Eval Object) ->
        (Object -> Object -> Eval Object) ->
        Object
mkOp opComplex opFloat opInt = primitive 2 $ \[x,y] ->
   case y of
      Complex {} -> opComplex x y
      Float {} -> opFloat x y
      Integer {} -> opInt x y
      other -> raise notImplementedError

add :: Object
add = mkOp addComplexComplexComplex addComplexFloatComplex addComplexIntComplex

sub :: Object
sub = mkOp subComplexComplexComplex subComplexFloatComplex subComplexIntComplex

mul :: Object
mul = mkOp mulComplexComplexComplex mulComplexFloatComplex mulComplexIntComplex

divide :: Object
divide = mkOp divComplexComplexComplex divComplexFloatComplex divComplexIntComplex

eq :: Object
eq = mkOp eqComplexComplexBool eqComplexFloatBool eqComplexIntBool

str :: Object
str = primitive 1 $ \[x] -> return $ string $ showComplex x

showComplex :: Object -> String
showComplex obj
   | r == 0 = if i < 0 then "-" ++ showImg else showImg
   | i < 0 = "(" ++ showR ++ "-" ++ showImg ++ ")"
   | otherwise = "(" ++ showR ++ "+" ++ showImg ++ ")"
   where
   showImg = showI ++ "j"
   showI = showNum $ abs i
   showR = showNum r
   c = object_complex obj
   i = imagPart c
   r = realPart c
   showNum :: Double -> String
   showNum n
      | fracPart == 0 = show intPart
      | otherwise = show n
      where
      (intPart, fracPart) = properFraction n :: (Integer, Double)
