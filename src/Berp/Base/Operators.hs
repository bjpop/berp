{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Operators
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Implementation of Python's operators. Where possible we should try to
-- specialise them to commonly used types.
--
-----------------------------------------------------------------------------

module Berp.Base.Operators 
   ( (+), (-), (*), (/), (==), (<), (>), (<=), (>=), (.), and, or, (%)
   , unaryMinus, unaryPlus, invert) 
   where

import Berp.Base.Prims (callMethod, raise)
import Prelude hiding ((+), (-), (*), (.), (/), (==), (<), (>), (<=), (>=), or, and)
import qualified Prelude ((==),(<),(>=),(*),(+),(-),(<=),(>))
import Berp.Base.Builtins.Exceptions (exception)
import Berp.Base.Object (lookupAttribute)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.StdTypes.Integer (int)
import Berp.Base.StdTypes.Bool (bool)
import Berp.Base.StdTypes.None (none)

infixl 9  .
infixl 7 *, /, %
infixl 6  +, -
infix  4  ==, <, <=, >=, >
infixr 3 `and`
infixr 2 `or`

-- XXX Really want to specialise some operations for particular types rather tham
-- going via the method lookups.

binop :: Hashed String -> Object -> Object -> Eval Object
binop str arg1 arg2 = callMethod arg1 str [arg2]

(%), (+), (-), (*), (/), (==), (<), (>), (<=), (>=), or, and :: Object -> Object -> Eval Object

(%) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 `Prelude.mod` object_integer obj2)
(%) x y = binop $(hashedStr "__mod__") x y

(+) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.+ object_integer obj2)
(+) x y = binop $(hashedStr "__add__") x y

(-) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.- object_integer obj2)
(-) x y = binop $(hashedStr "__sub__") x y

(*) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.* object_integer obj2)
(*) x y = binop $(hashedStr "__mul__") x y

(/) (Integer { object_integer = int1 }) (Integer { object_integer = int2 })
   | int2 Prelude.== 0 = raise exception >> return none
   | otherwise = return $ int (int1 `Prelude.div` int2)
(/) x y = binop $(hashedStr "__div__") x y

(<=) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.<= object_integer obj2)
(<=) x y = binop $(hashedStr "__le__") x y

(>) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.> object_integer obj2)
(>) x y = binop $(hashedStr "__gt__") x y

(==) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.== object_integer obj2)
(==) x y = binop $(hashedStr "__eq__") x y

(<) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.< object_integer obj2)
(<) x y = binop $(hashedStr "__lt__") x y

(>=) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.>= object_integer obj2)
(>=) x y = binop $(hashedStr "__ge__") x y

and obj1@(Bool {}) obj2@(Bool {}) = 
   return $ bool (object_bool obj1 Prelude.&& object_bool obj2)
and x y = binop $(hashedStr "__and__") x y

or obj1@(Bool {}) obj2@(Bool {}) = 
   return $ bool (object_bool obj1 Prelude.|| object_bool obj2)
or x y = binop $(hashedStr "__or__") x y

(.) :: Object -> Hashed String -> Eval Object
(.) object ident = lookupAttribute object ident

unaryMinus :: Object -> Eval Object
unaryMinus obj@(Integer {}) = return $ int $ negate $ object_integer obj
unaryMinus _other = error "unary minus applied to a non integer"

-- This is just the identity function
unaryPlus :: Object -> Eval Object
unaryPlus obj@(Integer {}) = return obj 
unaryPlus _other = error "unary plus applied to a non integer"

invert :: Object -> Eval Object
invert (Integer {}) = error "bitwise inversion not implemented" 
invert _other = error "unary invert applied to a non integer"
