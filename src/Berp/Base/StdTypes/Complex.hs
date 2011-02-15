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

{-# NOINLINE complex #-}
complex :: Double -> Double -> Object
complex r i = constantIO $ do
   identity <- newIdentity
   return $ Complex { object_identity = identity, object_real = r, object_imaginary = i }

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
{-
   , (divName, divide)
   , (ltName, lt)
   , (leName, le)
   , (gtName, gt)
   , (geName, ge)
-}
   , (eqName, eq)
   , (strName, str)
   ]

addComplex, subComplex, mulComplex :: Object -> Object -> Object
addComplex obj1 obj2
   = complex (object_real obj1 + object_real obj2)
             (object_imaginary obj1 + object_imaginary obj2)

subComplex obj1 obj2
   = complex (object_real obj1 - object_real obj2)
             (object_imaginary obj1 - object_imaginary obj2)

-- (r1 + i1)(r2 + i2) = (r1 r2 - i1 i2) + (i1 r2 + r1 i2)
mulComplex obj1 obj2
   = complex (r1 * r2 - i1 * i2) (i1 * r2 + r1 * i2)
   where
   r1 = object_real obj1
   r2 = object_real obj2
   i1 = object_imaginary obj1
   i2 = object_imaginary obj2

eqComplex :: Object -> Object -> Bool
eqComplex obj1 obj2
   = object_real obj1 == object_real obj2 &&
     object_imaginary obj1 == object_imaginary obj2

showComplex :: Object -> String
showComplex obj
   = "(" ++ show (object_real obj) ++
     "+" ++ show (object_imaginary obj) ++ "j)"

binOpComplex :: (Object -> Object -> Object) -> Object
binOpComplex f = primitive 2 $ \[x,y] -> binOp x y id f return

binOpBool :: (Object -> Object -> Bool) -> Object
binOpBool f = primitive 2 $ \[x,y] -> binOp x y id f (return . bool)

add :: Object
add = binOpComplex addComplex

sub :: Object
sub = binOpComplex subComplex

mul :: Object
mul = binOpComplex mulComplex

{-
divide :: Object
divide = binOpComplex (/)

lt :: Object
lt = binOpBool (<)

le :: Object
le = binOpBool (<=)

gt :: Object
gt = binOpBool (>)

ge :: Object
ge = binOpBool (>=)
-}

eq :: Object
eq = binOpBool eqComplex

str :: Object
str = primitive 1 $ \[x] -> return $ string $ showComplex x
