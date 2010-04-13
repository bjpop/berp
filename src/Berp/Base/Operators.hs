{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.Operators 
   ( (+), (-), (*), (/), (==), (<), (>), (<=), (>=), (.), and, or, (%)
   , unaryMinus, unaryPlus, invert) 
   where

import Berp.Base.Prims ((@@), callMethod)
import Prelude hiding ((+), (-), (*), (.), (/), (==), (<), (>), (<=), (>=), or, and)
import qualified Prelude ((==),(<),(>=),(/),(*),(+),(-),(<=),(>))
import Control.Monad.Trans (liftIO)
import Berp.Base.Ident (Ident)
import Berp.Base.Object (lookupAttribute)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Mangle (mangle)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.StdTypes.Integer (int)
import Berp.Base.StdTypes.Bool (bool)

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

(%), (+), (-), (*), (/), (==), (<), (>), (<=), (>=) :: Object -> Object -> Eval Object

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

(/) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 `Prelude.div` object_integer obj2)
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
(.) object ident = liftIO $ lookupAttribute object ident

unaryMinus :: Object -> Eval Object
unaryMinus obj@(Integer {}) = return $ int $ negate $ object_integer obj

-- This is just the identity function
unaryPlus :: Object -> Eval Object
unaryPlus obj@(Integer {}) = return obj 

invert :: Object -> Eval Object
invert obj@(Integer {}) = error "bitwise inversion not implemented" 
