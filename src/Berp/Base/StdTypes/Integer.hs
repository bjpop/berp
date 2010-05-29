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

{-# NOINLINE int #-}
int :: Integer -> Object 
int i = constantIO $ do
   identity <- newIdentity
   return $ Integer { object_identity = identity, object_integer = i }

{-# NOINLINE intClass #-}
intClass :: Object
intClass = constantIO $ do
   dict <- attributes
   newType [string "int", objectBase, dict]

attributes :: IO Object 
attributes = mkAttributes 
   [ (addName, add)
   , (subName, sub)
   , (mulName, mul)
   , (ltName, lt)
   , (leName, le)
   , (gtName, gt)
   , (geName, ge)
   , (eqName, eq)
   , (strName, str)
   , (modName, modulus)
   ]

binOpInteger :: (Integer -> Integer -> Integer) -> Object
binOpInteger f = primitive 2 $ \[x,y] -> binOp x y object_integer f (return . int)

binOpBool :: (Integer -> Integer -> Bool) -> Object 
binOpBool f = primitive 2 $ \[x,y] -> binOp x y object_integer f (return . bool)
        
add :: Object 
add = binOpInteger (+) 

sub :: Object 
sub = binOpInteger (-) 

mul :: Object 
mul = binOpInteger (*)

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

modulus :: Object
modulus = binOpInteger mod

str :: Object 
str = primitive 1 $ \[x] -> return $ string $ show $ object_integer x
-- str = primitive 1 $ \[x] -> return $ string "wazza"
