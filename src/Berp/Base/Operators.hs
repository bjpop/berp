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
import Berp.Base.Hash (Hashed)
import Berp.Base.StdTypes.Integer (int)
import Berp.Base.StdTypes.Bool (bool)
import Berp.Base.StdTypes.None (none)
import Berp.Base.StdNames 
   ( modName, addName, subName, mulName, divName, leName
   , gtName, eqName, ltName, geName, orName, andName)

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
(%) x y = binop modName x y

(+) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.+ object_integer obj2)
(+) x y = binop addName x y

(-) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.- object_integer obj2)
(-) x y = binop subName x y

(*) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.* object_integer obj2)
(*) x y = binop mulName x y

(/) (Integer { object_integer = int1 }) (Integer { object_integer = int2 })
   | int2 Prelude.== 0 = raise exception >> return none
   | otherwise = return $ int (int1 `Prelude.div` int2)
(/) x y = binop divName x y

(<=) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.<= object_integer obj2)
(<=) x y = binop leName x y

(>) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.> object_integer obj2)
(>) x y = binop gtName x y

(==) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.== object_integer obj2)
(==) x y = binop eqName x y

(<) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.< object_integer obj2)
(<) x y = binop ltName x y

(>=) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ bool (object_integer obj1 Prelude.>= object_integer obj2)
(>=) x y = binop geName x y

and obj1@(Bool {}) obj2@(Bool {}) = 
   return $ bool (object_bool obj1 Prelude.&& object_bool obj2)
and x y = binop andName x y

or obj1@(Bool {}) obj2@(Bool {}) = 
   return $ bool (object_bool obj1 Prelude.|| object_bool obj2)
or x y = binop orName x y

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
