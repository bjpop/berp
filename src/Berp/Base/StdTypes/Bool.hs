-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Bool
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard boolean type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Bool (bool, true, false, boolClass) where

import Prelude hiding (and, or)
import Berp.Base.Monad (constantIO)
import Berp.Base.Prims (primitive, raise)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Builtins (typeError, notImplementedError)
import Berp.Base.StdNames
import Berp.Base.Truth (truth)
import qualified Berp.Base.Operators as Op ( and, or )
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.ObjectBase (objectBase)

bool :: Bool -> Object
bool True = true
bool False = false

{-# NOINLINE true #-}
{-# NOINLINE false #-}
true, false :: Object
true =
   constantIO $ do
      identity <- newIdentity
      return $ Bool { object_identity = identity, object_bool = True }
false =
   constantIO $ do
      identity <- newIdentity
      return $ Bool { object_identity = identity, object_bool = False }

{-# NOINLINE boolClass #-}
boolClass :: Object
boolClass = constantIO $ do
   dict <- attributes
   theType <- newType [string "bool", objectBase, dict]
   return $ theType { object_constructor = mkBool }
   where
   mkBool :: [Object] -> Eval Object
   mkBool [] = return false
   mkBool [x] =
      case truth x of
         True -> return true
         False -> return false
   mkBool _other = raise typeError

attributes :: IO Object
attributes = mkAttributes
   [ (andName, and)
   , (orName, or)
   , (strName, str)
   ]

mkOp :: (Object -> Object -> Eval Object) -> Object
mkOp op = primitive 2 $ \[x,y] ->
   case y of
      Bool {} -> op x y
      _other -> raise notImplementedError

and :: Object
and = mkOp Op.and

or :: Object
or = mkOp Op.or

str :: Object
str = primitive 1 $ \[x] -> Prelude.return $ string $ show $ object_bool x
