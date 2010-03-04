{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.Operators ((+), (-), (*), (/), (==), (<), (>), (<=), (>=), (.), and, or) where

import Berp.Base.Prims ((@@), callMethod)
import Prelude hiding ((+), (-), (*), (.), (/), (==), (<), (>), (<=), (>=), or, and)
import qualified Prelude ((+),(-),(<=),(>))
import Control.Monad.Trans (liftIO)
import Berp.Base.Ident (Ident)
import Berp.Base.Object (lookupAttribute)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Mangle (mangle)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.StdTypes.Integer (int)

infixl 9  .
infixl 7 *, /
infixl 6  +, -
infix  4  ==, <, <=, >=, >
infixr 3 `and`
infixr 2 `or`

-- XXX Really want to specialise some operations for particular types rather tham
-- going via the method lookups.

binop :: Hashed String -> Object -> Object -> Eval Object
binop str arg1 arg2 = callMethod arg1 str [arg2]
{-
binop :: Hashed String -> Object -> Object -> Eval Object
binop opName arg1 arg2 = do
    procObj <- liftIO $ lookupAttribute arg1 $ opName
    procObj @@ [arg1, arg2]
-}

(+), (-), (*), (/), (==), (<), (>), (<=), (>=) :: Object -> Object -> Eval Object

(+) = binop $(hashedStr "__add__")
{-
(+) x _ = do liftIO $ putStrLn "calling plus"
             return x
-}
{-
(+) obj1@(Integer {}) obj2@(Integer {}) = 
   return $ int (object_integer obj1 Prelude.+ object_integer obj2)
-}

{-
(+) comp1 comp2 = do
   x <- comp1
   y <- comp2
   return (x { object_integer = object_integer x Prelude.+ object_integer y })
-}


(-) = binop $(hashedStr "__sub__")
{-
(-) comp1 comp2 = do
   x <- comp1
   y <- comp2
   return (x { object_integer = object_integer x Prelude.- object_integer y })
-}



(<=) = binop $(hashedStr "__le__")
{-
(<=) comp1 comp2 = do
   x <- comp1
   y <- comp2
   return (BoolObject { object_bool = object_integer x Prelude.<= object_integer y })
-}


(>) = binop $(hashedStr "__gt__")
{-
(>) comp1 comp2 = do
   x <- comp1
   y <- comp2
   return (BoolObject { object_bool = object_integer x Prelude.> object_integer y })
-}

(*) = binop $(hashedStr "__mul__")
(/) = binop $(hashedStr "__div__")
(==) = binop $(hashedStr "__eq__")
(<) = binop $(hashedStr "__lt__")
(>=) = binop $(hashedStr "__ge__")
and = binop $(hashedStr "__and__")
or = binop $(hashedStr "__or__")

(.) :: Object -> Hashed String -> Eval Object
(.) object ident = liftIO $ lookupAttribute object ident
