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
import Berp.Base.Prims (primitive, raiseException, ret)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import Berp.Base.Truth (truth)
import qualified Berp.Base.Operators as Op ( and, or )
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.ObjectBase (objectBase)

bool :: Bool -> Object
bool True = true
bool False = false

true, false :: Object
true = TrueObject
false = FalseObject

boolClass :: Eval Object
boolClass = do
   dict <- attributes
   base <- objectBase
   theType <- newType [string "bool", base, dict]
   return $ theType { object_constructor = mkBool }
   where
   mkBool :: [Object] -> Eval Object
   mkBool [] = ret false
   mkBool [x] = do
      isTrue <- truth x
      if isTrue then ret true else ret false
   mkBool _other = raiseException "typeError"

attributes :: Eval Object
attributes = mkAttributesList
   [ (specialAndName, and)
   , (specialOrName, or)
   , (specialStrName, str)
   ]

mkOp :: (Object -> Object -> Eval Object) -> Eval Object
mkOp op = primitive 2 fun
   where
   fun (x:y:_) =
      case y of
         TrueObject {} -> op x y
         FalseObject {} -> op x y
         _other -> raiseException "notImplementedError"
   fun _other = error "operator on Bool applied to wrong number of arguments"

and :: Eval Object
and = mkOp Op.and

or :: Eval Object
or = mkOp Op.or

str :: Eval Object
str = primitive 1 fun
   where
   fun (TrueObject:_) = Prelude.return $ string "True"
   fun (FalseObject:_) = Prelude.return $ string "False"
   fun _other = error "str method on Bool applied to wrong number of arguments"
