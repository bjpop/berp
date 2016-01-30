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
-- Note: Complex numbers intentionally don't have an ordering.
--
-----------------------------------------------------------------------------

module Berp.Base.Operators
   ( (+), (-), (*), (/), (==), (<), (>), (<=), (>=), (.), and, or, (%)
   , unaryMinus, unaryPlus, invert, not

   , modIntIntInt
   , addIntIntInt
   , addIntFloatFloat
   , addIntComplexComplex
   , subIntIntInt
   , subIntFloatFloat
   , subIntComplexComplex
   , mulIntIntInt
   , mulIntFloatFloat
   , mulIntComplexComplex
   , divIntIntInt
   , divIntFloatFloat
   , divIntComplexComplex
   , leIntIntBool
   , leIntFloatBool
   , gtIntIntBool
   , gtIntFloatBool
   , eqIntIntBool
   , eqIntFloatBool
   , eqIntComplexBool
   , ltIntIntBool
   , ltIntFloatBool
   , geIntIntBool
   , geIntFloatBool

   , addFloatFloatFloat
   , addFloatIntFloat
   , addFloatComplexComplex
   , subFloatFloatFloat
   , subFloatIntFloat
   , subFloatComplexComplex
   , mulFloatFloatFloat
   , mulFloatIntFloat
   , mulFloatComplexComplex
   , divFloatFloatFloat
   , divFloatIntFloat
   , divFloatComplexComplex
   , leFloatFloatBool
   , leFloatIntBool
   , gtFloatFloatBool
   , gtFloatIntBool
   , eqFloatFloatBool
   , eqFloatIntBool
   , eqFloatComplexBool
   , ltFloatFloatBool
   , ltFloatIntBool
   , geFloatFloatBool
   , geFloatIntBool

   , addComplexComplexComplex
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
   , eqComplexFloatBool

   )
   where

import Data.Complex (Complex (..))
import Prelude hiding ((+), (-), (*), (.), (/), (==), (<), (>), (<=), (>=), or, and, not)
import qualified Prelude ((==),(<),(>=),(*),(+),(-),(<=),(>),(.),(/), not)
import Berp.Base.Prims (callMethod, raise)
import Berp.Base.Builtins.Exceptions (typeError, zeroDivisionError)
import Berp.Base.Object (lookupAttribute)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Hash (Hashed)
import Berp.Base.StdNames
   ( specialModName, specialAddName, specialSubName, specialMulName, specialDivName, specialLeName
   , specialGtName, specialEqName, specialLtName, specialGeName)
import Berp.Base.Truth (truth)
import {-# SOURCE #-} Berp.Base.StdTypes.Integer (int)
import {-# SOURCE #-} Berp.Base.StdTypes.Float (float)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (bool, true, false)
import {-# SOURCE #-} Berp.Base.StdTypes.Complex (complex)

infixl 9  .
infixl 7 *, /, %
infixl 6  +, -
infix  4  ==, <, <=, >=, >
infixr 3 `and`
infixr 2 `or`

{-
  Is it possible to minimise the boiler plate?
  Maybe Template Haskell?
-}

-- We specialise some operations for particular types rather than
-- going via the method lookups. Fall back to method look ups for the
-- general case.

binop :: Hashed String -> Object -> Object -> Eval Object
binop str arg1 arg2 = callMethod arg1 str [arg2]

specialiseOp :: (Object -> a) -> (Object -> a) ->
                (a -> a -> r) -> (r -> Object) -> Object -> Object -> Eval Object
specialiseOp project1 project2 op build obj1 obj2 =
   return $ build (project1 obj1 `op` project2 obj2)

specialiseOpIntIntInt :: (Integer -> Integer -> Integer) -> Object -> Object -> Eval Object
specialiseOpIntIntInt op = specialiseOp object_integer object_integer op int

specialiseOpIntIntBool :: (Integer -> Integer -> Bool) -> Object -> Object -> Eval Object
specialiseOpIntIntBool op = specialiseOp object_integer object_integer op bool

{-
specialiseOpBoolBoolBool :: (Bool -> Bool -> Bool) -> Object -> Object -> Eval Object
specialiseOpBoolBoolBool op = specialiseOp object_bool object_bool op bool
-}

specialiseOpFloatFloatFloat :: (Double -> Double -> Double) -> Object -> Object -> Eval Object
specialiseOpFloatFloatFloat op = specialiseOp object_float object_float op float

specialiseOpFloatFloatBool :: (Double -> Double -> Bool) -> Object -> Object -> Eval Object
specialiseOpFloatFloatBool op = specialiseOp object_float object_float op bool

specialiseOpIntFloatFloat :: (Double -> Double -> Double) -> Object -> Object -> Eval Object
specialiseOpIntFloatFloat op =
   specialiseOp (fromInteger Prelude.. object_integer) object_float op float

specialiseOpIntFloatBool :: (Double -> Double -> Bool) -> Object -> Object -> Eval Object
specialiseOpIntFloatBool op =
   specialiseOp (fromInteger Prelude.. object_integer) object_float op bool

specialiseOpFloatIntFloat :: (Double -> Double -> Double) -> Object -> Object -> Eval Object
specialiseOpFloatIntFloat op =
   specialiseOp object_float (fromInteger Prelude.. object_integer) op float

specialiseOpFloatIntBool :: (Double -> Double -> Bool) -> Object -> Object -> Eval Object
specialiseOpFloatIntBool op =
   specialiseOp object_float (fromInteger Prelude.. object_integer) op bool

type ComplexD = Complex Double

specialiseOpComplexComplexComplex :: (ComplexD -> ComplexD -> ComplexD) -> Object -> Object -> Eval Object
specialiseOpComplexComplexComplex op = specialiseOp object_complex object_complex op complex

specialiseOpComplexComplexBool :: (ComplexD -> ComplexD -> Bool) -> Object -> Object -> Eval Object
specialiseOpComplexComplexBool op = specialiseOp object_complex object_complex op bool

specialiseOpIntComplexBool :: (ComplexD -> ComplexD -> Bool) -> Object -> Object -> Eval Object
specialiseOpIntComplexBool op = specialiseOp (fromInteger Prelude.. object_integer) object_complex op bool

specialiseOpComplexIntBool :: (ComplexD -> ComplexD -> Bool) -> Object -> Object -> Eval Object
specialiseOpComplexIntBool op = specialiseOp object_complex (fromInteger Prelude.. object_integer) op bool

specialiseOpFloatComplexBool :: (ComplexD -> ComplexD -> Bool) -> Object -> Object -> Eval Object
specialiseOpFloatComplexBool op = specialiseOp (realToFrac Prelude.. object_float) object_complex op bool

specialiseOpComplexFloatBool :: (ComplexD -> ComplexD -> Bool) -> Object -> Object -> Eval Object
specialiseOpComplexFloatBool op = specialiseOp object_complex (realToFrac Prelude.. object_float) op bool

specialiseOpFloatComplexComplex :: (ComplexD -> ComplexD -> ComplexD) -> Object -> Object -> Eval Object
specialiseOpFloatComplexComplex op =
   specialiseOp (realToFrac Prelude.. object_float) object_complex op complex

specialiseOpComplexFloatComplex :: (ComplexD -> ComplexD -> ComplexD) -> Object -> Object -> Eval Object
specialiseOpComplexFloatComplex op =
   specialiseOp object_complex (realToFrac Prelude.. object_float) op complex

specialiseOpIntComplexComplex :: (ComplexD -> ComplexD -> ComplexD) -> Object -> Object -> Eval Object
specialiseOpIntComplexComplex op =
   specialiseOp (fromInteger Prelude.. object_integer) object_complex op complex

specialiseOpComplexIntComplex :: (ComplexD -> ComplexD -> ComplexD) -> Object -> Object -> Eval Object
specialiseOpComplexIntComplex op =
   specialiseOp object_complex (fromInteger Prelude.. object_integer) op complex

(%), (+), (-), (*), (/), (==), (<), (>), (<=), (>=), or, and :: Object -> Object -> Eval Object

modIntIntInt :: Object -> Object -> Eval Object
modIntIntInt = specialiseOpIntIntInt (Prelude.mod)

-- XXX fixme
(%) obj1@(Integer {}) obj2@(Integer {}) = modIntIntInt obj1 obj2
(%) x y = binop specialModName x y

addIntIntInt, addFloatFloatFloat, addIntFloatFloat, addFloatIntFloat, addComplexComplexComplex, addIntComplexComplex, addComplexIntComplex, addFloatComplexComplex, addComplexFloatComplex :: Object -> Object -> Eval Object

addIntIntInt = specialiseOpIntIntInt (Prelude.+)
addFloatFloatFloat = specialiseOpFloatFloatFloat (Prelude.+)
addIntFloatFloat = specialiseOpIntFloatFloat (Prelude.+)
addFloatIntFloat = specialiseOpFloatIntFloat (Prelude.+)
addComplexComplexComplex = specialiseOpComplexComplexComplex (Prelude.+)
addIntComplexComplex = specialiseOpIntComplexComplex (Prelude.+)
addComplexIntComplex = specialiseOpComplexIntComplex (Prelude.+)
addFloatComplexComplex = specialiseOpFloatComplexComplex (Prelude.+)
addComplexFloatComplex = specialiseOpComplexFloatComplex (Prelude.+)

(+) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> addIntIntInt obj1 obj2
      Float {} -> addIntFloatFloat obj1 obj2
      Complex {} -> addIntComplexComplex obj1 obj2
      _other -> raise typeError
(+) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> addFloatFloatFloat obj1 obj2
      Integer {} -> addFloatIntFloat obj1 obj2
      Complex {} -> addFloatComplexComplex obj1 obj2
      _other -> raise typeError
(+) obj1@(Complex {}) obj2 =
   case obj2 of
      Complex {} -> addComplexComplexComplex obj1 obj2
      Integer {} -> addComplexIntComplex obj1 obj2
      Float {} -> addComplexFloatComplex obj1 obj2
      _other -> raise typeError
(+) x y = binop specialAddName x y

subIntIntInt, subFloatFloatFloat, subIntFloatFloat, subFloatIntFloat, subComplexComplexComplex, subIntComplexComplex, subComplexIntComplex, subFloatComplexComplex, subComplexFloatComplex :: Object -> Object -> Eval Object

subIntIntInt = specialiseOpIntIntInt (Prelude.-)
subFloatFloatFloat = specialiseOpFloatFloatFloat (Prelude.-)
subIntFloatFloat = specialiseOpIntFloatFloat (Prelude.-)
subFloatIntFloat = specialiseOpFloatIntFloat (Prelude.-)
subComplexComplexComplex = specialiseOpComplexComplexComplex (Prelude.-)
subIntComplexComplex = specialiseOpIntComplexComplex (Prelude.-)
subComplexIntComplex = specialiseOpComplexIntComplex (Prelude.-)
subFloatComplexComplex = specialiseOpFloatComplexComplex (Prelude.-)
subComplexFloatComplex = specialiseOpComplexFloatComplex (Prelude.-)

(-) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> subIntIntInt obj1 obj2
      Float {} -> subIntFloatFloat obj1 obj2
      Complex {} -> subIntComplexComplex obj1 obj2
      _other -> raise typeError
(-) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> subFloatFloatFloat obj1 obj2
      Integer {} -> subFloatIntFloat obj1 obj2
      Complex {} -> subFloatComplexComplex obj1 obj2
      _other -> raise typeError
(-) obj1@(Complex {}) obj2 =
   case obj2 of
      Complex {} -> subComplexComplexComplex obj1 obj2
      Integer {} -> subComplexIntComplex obj1 obj2
      Float {} -> subComplexFloatComplex obj1 obj2
      _other -> raise typeError
(-) x y = binop specialSubName x y

mulIntIntInt, mulFloatFloatFloat, mulIntFloatFloat, mulFloatIntFloat, mulComplexComplexComplex, mulIntComplexComplex, mulComplexIntComplex, mulFloatComplexComplex, mulComplexFloatComplex :: Object -> Object -> Eval Object

mulIntIntInt = specialiseOpIntIntInt (Prelude.*)
mulFloatFloatFloat = specialiseOpFloatFloatFloat (Prelude.*)
mulIntFloatFloat = specialiseOpIntFloatFloat (Prelude.*)
mulFloatIntFloat = specialiseOpFloatIntFloat (Prelude.*)
mulComplexComplexComplex = specialiseOpComplexComplexComplex (Prelude.*)
mulIntComplexComplex = specialiseOpIntComplexComplex (Prelude.*)
mulComplexIntComplex = specialiseOpComplexIntComplex (Prelude.*)
mulFloatComplexComplex = specialiseOpFloatComplexComplex (Prelude.*)
mulComplexFloatComplex = specialiseOpComplexFloatComplex (Prelude.*)

(*) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> mulIntIntInt obj1 obj2
      Float {} -> mulIntFloatFloat obj1 obj2
      Complex {} -> mulIntComplexComplex obj1 obj2
      _other -> raise typeError
(*) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> mulFloatFloatFloat obj1 obj2
      Integer {} -> mulFloatIntFloat obj1 obj2
      Complex {} -> mulFloatComplexComplex obj1 obj2
      _other -> raise typeError
(*) obj1@(Complex {}) obj2 =
   case obj2 of
      Complex {} -> mulComplexComplexComplex obj1 obj2
      Integer {} -> mulComplexIntComplex obj1 obj2
      Float {} -> mulComplexFloatComplex obj1 obj2
      _other -> raise typeError
(*) x y = binop specialMulName x y

checkDivByZero :: (Num a, Eq a) => a -> Eval Object -> Eval Object
checkDivByZero denom comp
   | denom Prelude.== 0 = raise zeroDivisionError
   | otherwise = comp

divIntIntInt, divFloatFloatFloat, divIntFloatFloat, divFloatIntFloat, divComplexComplexComplex, divIntComplexComplex, divComplexIntComplex, divFloatComplexComplex, divComplexFloatComplex :: Object -> Object -> Eval Object

divIntIntInt obj1 obj2 =
   checkDivByZero (object_integer obj2) $ specialiseOpIntIntInt (Prelude.div) obj1 obj2
divFloatFloatFloat obj1 obj2 =
   checkDivByZero (object_float obj2) $ specialiseOpFloatFloatFloat (Prelude./) obj1 obj2
divIntFloatFloat obj1 obj2 =
   checkDivByZero (object_float obj2) $ specialiseOpIntFloatFloat (Prelude./) obj1 obj2
divFloatIntFloat obj1 obj2 =
   checkDivByZero (object_integer obj2) $ specialiseOpFloatIntFloat (Prelude./) obj1 obj2
divComplexComplexComplex obj1 obj2 =
   checkDivByZero (object_complex obj2) $ specialiseOpComplexComplexComplex (Prelude./) obj1 obj2
divIntComplexComplex obj1 obj2 =
   checkDivByZero (object_complex obj2) $ specialiseOpIntComplexComplex (Prelude./) obj1 obj2
divComplexIntComplex obj1 obj2 =
   checkDivByZero (object_integer obj2) $ specialiseOpComplexIntComplex (Prelude./) obj1 obj2
divFloatComplexComplex obj1 obj2 =
   checkDivByZero (object_complex obj2) $ specialiseOpFloatComplexComplex (Prelude./) obj1 obj2
divComplexFloatComplex obj1 obj2 =
   checkDivByZero (object_float obj2) $ specialiseOpComplexFloatComplex (Prelude./) obj1 obj2

(/) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> divIntIntInt obj1 obj2
      Float {} -> divIntFloatFloat obj1 obj2
      Complex {} -> divIntComplexComplex obj1 obj2
      _other -> raise typeError
(/) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> divFloatFloatFloat obj1 obj2
      Integer {} -> divFloatIntFloat obj1 obj2
      Complex {} -> divFloatComplexComplex obj1 obj2
      _other -> raise typeError
(/) obj1@(Complex {}) obj2 =
   case obj2 of
      Complex {} -> divComplexComplexComplex obj1 obj2
      Integer {} -> divComplexIntComplex obj1 obj2
      Float {} -> divComplexFloatComplex obj1 obj2
      _other -> raise typeError
(/) x y = binop specialDivName x y

leIntIntBool, leFloatFloatBool, leIntFloatBool, leFloatIntBool :: Object -> Object -> Eval Object

leIntIntBool = specialiseOpIntIntBool (Prelude.<=)
leFloatFloatBool = specialiseOpFloatFloatBool (Prelude.<=)
leIntFloatBool = specialiseOpIntFloatBool (Prelude.<=)
leFloatIntBool = specialiseOpFloatIntBool (Prelude.<=)

(<=) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> leIntIntBool obj1 obj2
      Float {} -> leIntFloatBool obj1 obj2
      _other -> raise typeError
(<=) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> leFloatFloatBool obj1 obj2
      Integer {} -> leIntFloatBool obj1 obj2
      _other -> raise typeError
(<=) x y = binop specialLeName x y

gtIntIntBool, gtFloatFloatBool, gtIntFloatBool, gtFloatIntBool :: Object -> Object -> Eval Object

gtIntIntBool = specialiseOpIntIntBool (Prelude.>)
gtFloatFloatBool = specialiseOpFloatFloatBool (Prelude.>)
gtIntFloatBool = specialiseOpIntFloatBool (Prelude.>)
gtFloatIntBool = specialiseOpFloatIntBool (Prelude.>)

(>) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> gtIntIntBool obj1 obj2
      Float {} -> gtIntFloatBool obj1 obj2
      _other -> raise typeError
(>) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> gtFloatFloatBool obj1 obj2
      Integer {} -> gtIntFloatBool obj1 obj2
      _other -> raise typeError
(>) x y = binop specialGtName x y

eqIntIntBool, eqFloatFloatBool, eqIntFloatBool, eqFloatIntBool, eqComplexComplexBool, eqIntComplexBool, eqComplexIntBool, eqFloatComplexBool, eqComplexFloatBool :: Object -> Object -> Eval Object

eqIntIntBool = specialiseOpIntIntBool (Prelude.==)
eqFloatFloatBool = specialiseOpFloatFloatBool (Prelude.==)
eqIntFloatBool = specialiseOpIntFloatBool (Prelude.==)
eqFloatIntBool = specialiseOpFloatIntBool (Prelude.==)
eqComplexComplexBool = specialiseOpComplexComplexBool (Prelude.==)
eqIntComplexBool = specialiseOpIntComplexBool (Prelude.==)
eqComplexIntBool = specialiseOpComplexIntBool (Prelude.==)
eqFloatComplexBool = specialiseOpFloatComplexBool (Prelude.==)
eqComplexFloatBool = specialiseOpComplexFloatBool (Prelude.==)

(==) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> eqIntIntBool obj1 obj2
      Float {} -> eqIntFloatBool obj1 obj2
      Complex {} -> eqIntComplexBool obj1 obj2
      _other -> raise typeError
(==) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> eqFloatFloatBool obj1 obj2
      Integer {} -> eqIntFloatBool obj1 obj2
      Complex {} -> eqFloatComplexBool obj1 obj2
      _other -> raise typeError
(==) obj1@(Complex {}) obj2 =
   case obj2 of
      Complex {} -> eqComplexComplexBool obj1 obj2
      Integer {} -> eqComplexIntBool obj1 obj2
      Float {} -> eqComplexFloatBool obj1 obj2
      _other -> raise typeError
(==) x y = binop specialEqName x y

ltIntIntBool, ltFloatFloatBool, ltIntFloatBool, ltFloatIntBool :: Object -> Object -> Eval Object

ltIntIntBool = specialiseOpIntIntBool (Prelude.<)
ltFloatFloatBool = specialiseOpFloatFloatBool (Prelude.<)
ltIntFloatBool = specialiseOpIntFloatBool (Prelude.<)
ltFloatIntBool = specialiseOpFloatIntBool (Prelude.<)

(<) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> ltIntIntBool obj1 obj2
      Float {} -> ltIntFloatBool obj1 obj2
      _other -> raise typeError
(<) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> ltFloatFloatBool obj1 obj2
      Integer {} -> ltIntFloatBool obj1 obj2
      _other -> raise typeError
(<) x y = binop specialLtName x y

geIntIntBool, geFloatFloatBool, geIntFloatBool, geFloatIntBool :: Object -> Object -> Eval Object

geIntIntBool = specialiseOpIntIntBool (Prelude.>=)
geFloatFloatBool = specialiseOpFloatFloatBool (Prelude.>=)
geIntFloatBool = specialiseOpIntFloatBool (Prelude.>=)
geFloatIntBool = specialiseOpFloatIntBool (Prelude.>=)

(>=) obj1@(Integer {}) obj2 =
   case obj2 of
      Integer {} -> geIntIntBool obj1 obj2
      Float {} -> geIntFloatBool obj1 obj2
      _other -> raise typeError
(>=) obj1@(Float {}) obj2 =
   case obj2 of
      Float {} -> geFloatFloatBool obj1 obj2
      Integer {} -> geIntFloatBool obj1 obj2
      _other -> raise typeError
(>=) x y = binop specialGeName x y

{-
   From the Python Language Reference, sec 5.10 "Boolean Operations"
   The expression x and y first evaluates x; if x is false, its value
   is returned; otherwise, y is evaluated and the resulting value is
   returned.
-}
and obj1 obj2
   | truth obj1 = return obj2
   | otherwise  = return obj1

{-
   The expression x or y first evaluates x; if x is true, its value 
   is returned; otherwise, y is evaluated and the resulting value 
   is returned.
-}

or obj1 obj2
   | truth obj1 = return obj1
   | otherwise  = return obj2

(.) :: Object -> Hashed String -> Eval Object
(.) object ident = lookupAttribute object ident

unaryMinus :: Object -> Eval Object
unaryMinus obj@(Integer {}) = return $ int $ negate $ object_integer obj
unaryMinus obj@(Float {}) = return $ float $ negate $ object_float obj
unaryMinus obj@(Complex {}) = return $ complex $ negate $ object_complex obj
unaryMinus _other = error "unary minus applied to a non integer"

-- This is just the identity function
unaryPlus :: Object -> Eval Object
unaryPlus obj@(Integer {}) = return obj
unaryPlus obj@(Float {}) = return obj
unaryPlus obj@(Complex {}) = return obj
-- XXX in CPython this turns the boolean into an integer
unaryPlus obj@(Bool {}) = return obj
unaryPlus _other = error "unary plus applied to a non integer"

invert :: Object -> Eval Object
invert (Integer {}) = error "bitwise inversion not implemented"
invert _other = error "unary invert applied to a non integer"

not :: Object -> Eval Object
not obj
   | Prelude.not $ truth obj = return true
   | otherwise = return false
