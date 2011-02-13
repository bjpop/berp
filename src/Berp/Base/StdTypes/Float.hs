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
import Berp.Base.Prims (binOp, primitive)
import Berp.Base.SemanticTypes (Object (..))
import Berp.Base.StdTypes.Bool (bool)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
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
attributes = mkAttributes
   [ (addName, add)
   , (subName, sub)
   , (mulName, mul)
   , (divName, divide)
   , (ltName, lt)
   , (leName, le)
   , (gtName, gt)
   , (geName, ge)
   , (eqName, eq)
   , (strName, str)
   ]

binOpFloat :: (Double -> Double -> Double) -> Object
binOpFloat f = primitive 2 $ \[x,y] -> binOp x y object_float f (return . float)

binOpBool :: (Double -> Double -> Bool) -> Object
binOpBool f = primitive 2 $ \[x,y] -> binOp x y object_float f (return . bool)

add :: Object
add = binOpFloat (+)

sub :: Object
sub = binOpFloat (-)

mul :: Object
mul = binOpFloat (*)

divide :: Object
divide = binOpFloat (/)

lt :: Object
lt = binOpBool (<)

le :: Object
le = binOpBool (<=)

gt :: Object
gt = binOpBool (>)

ge :: Object
ge = binOpBool (>=)

eq :: Object
eq = binOpBool (==)

str :: Object
str = primitive 1 $ \[x] -> return $ string $ show $ object_float x
