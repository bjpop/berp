{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Integer (int, intClass) where

import Control.Monad.Trans (liftIO)
import Berp.Base.Monad (constantIO)
import Berp.Base.Prims (binOp, primitive)
import Berp.Base.SemanticTypes (Eval, Procedure, Object (..))
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Bool (bool)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames 
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

-- needed for the Num instance
instance Eq Object where
   (==) = undefined 

-- needed for overloaded numeric literals
instance Num Object where
    fromInteger = int
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined

{-# NOINLINE int #-}
int :: Integer -> Object 
int i = constantIO $ do
   identity <- newIdentity
   return $ Integer { object_identity = identity, object_integer = i }

{-# NOINLINE intClass #-}
intClass :: Object
intClass = constantIO $ do
   identity <- newIdentity
   as <- attributes
   dict <- attributes
   return $
      Type 
      { object_identity = identity
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = objectBase 
      , object_constructor = \_ -> return (int 0)
      , object_type_name = string "int"
      }

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
