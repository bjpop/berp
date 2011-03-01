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
import Berp.Base.Prims (primitive, raise)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Builtins (notImplementedError)
import Berp.Base.StdNames
import Berp.Base.Operators
   ( addIntIntInt, subIntIntInt, modIntIntInt, mulIntIntInt, ltIntIntBool
   , leIntIntBool, gtIntIntBool, geIntIntBool, eqIntIntBool, divIntIntInt)
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
   , (divName, divide)
   , (ltName, lt)
   , (leName, le)
   , (gtName, gt)
   , (geName, ge)
   , (eqName, eq)
   , (strName, str)
   , (modName, modulus)
   ]

mkOp :: (Object -> Object -> Eval Object) -> Object
mkOp op = primitive 2 $ \[x,y] ->
   case y of
      Integer {} -> op x y
      _other -> raise notImplementedError

add :: Object
add = mkOp addIntIntInt

sub :: Object
sub = mkOp subIntIntInt

mul :: Object
mul = mkOp mulIntIntInt

divide :: Object
divide = mkOp divIntIntInt

lt :: Object
lt = mkOp ltIntIntBool

le :: Object
le = mkOp leIntIntBool

gt :: Object
gt = mkOp gtIntIntBool

ge :: Object
ge = mkOp geIntIntBool

eq :: Object
eq = mkOp eqIntIntBool

modulus :: Object
modulus = mkOp modIntIntInt

str :: Object
str = primitive 1 $ \[x] -> return $ string $ show $ object_integer x
