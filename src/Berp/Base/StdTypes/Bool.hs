{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Bool (bool, true, false, boolClass) where

import Prelude hiding (and, or)
import Control.Applicative ((<$>))
import Berp.Base.Monad (constantIO)
import Berp.Base.Prims (binOp, primitive)
import Berp.Base.SemanticTypes (Eval, Procedure, Object (..))
import Berp.Base.StdTypes.String (string)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

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
   identity <- newIdentity
   dict <- attributes
   newType [string "bool", objectBase, dict]
{-
   return $
      Type 
      { object_identity = identity
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = objectBase 
      , object_constructor = \_ -> return false 
      , object_type_name = string "bool"
      }
-}
    
attributes :: IO Object 
attributes = mkAttributes 
   [ (andName, and)
   , (orName, or)
   , (strName, str)
   ]

-- XXX this doesn't look safe to me. What if wrong number of arguments? Or 
-- argument objects are not booleans?
binOpBool :: (Bool -> Bool -> Bool) -> Object 
binOpBool f = primitive 2 $ \[x,y] -> binOp x y object_bool f (Prelude.return . bool)

and :: Object 
and = binOpBool (&&) 

or :: Object 
or = binOpBool (||)

str :: Object 
str = primitive 1 $ \[x] -> Prelude.return $ string $ show $ object_bool x
