{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.Tuple (tuple, tupleClass, emptyTuple) where

import Data.List (intersperse)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Procedure, Object (..))
import Berp.Base.Prims (callMethod, primitive)
import Berp.Base.StdTypes.String (string)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Hash (hashedStr)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (typeClass)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
-- import {-# SOURCE #-} Berp.Base.StdTypes.Primitive (primitive)

emptyTuple :: Object
emptyTuple = tuple []

{-# NOINLINE tuple #-}
tuple :: [Object] -> Object
tuple elements = constantIO $ do 
   identity <- newIdentity
   return $ 
      Tuple
      { object_identity = identity
      , object_tuple = elements
      , object_length = length elements
      }

{-# NOINLINE tupleClass #-}
tupleClass :: Object
tupleClass = constantIO $ do 
   identity <- newIdentity
   dict <- attributes
   return $
      Type 
      { object_identity = identity
      , object_type = typeClass
      , object_dict = dict 
      , object_bases = objectBase
      , object_constructor = \ _ -> return emptyTuple 
      , object_type_name = string "tuple"
      }

attributes :: IO Object 
attributes = mkAttributes 
   [ (eqName, eq)
   , (strName, str)
   ]

eq :: Object 
eq = error "== on tuple not defined"

str :: Object 
str = primitive 1 $ \[x] -> do
   objStrs <- mapM objectToStr $ object_tuple x
   let strings = map object_string objStrs
   case strings of
      [oneString] -> return $ string $ "(" ++ oneString ++ ",)"
      _other -> return $ string $ "(" ++ concat (intersperse ", " strings) ++ ")"
   where
   objectToStr obj = callMethod obj strName [] 
