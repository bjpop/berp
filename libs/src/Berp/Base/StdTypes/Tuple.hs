-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Tuple
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard tuple type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Tuple (tuple, tupleClass, emptyTuple, getTupleElements) where

import Data.List (intersperse)
import Berp.Base.SemanticTypes (Object (..), Eval)
import Berp.Base.Prims (primitive, showObject)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

emptyTuple :: Eval Object
emptyTuple = tuple []

tuple :: [Object] -> Eval Object
tuple elements = do
   identity <- newIdentity
   return $
      Tuple
      { object_identity = identity
      , object_tuple = elements
      , object_length = length elements
      }

tupleClass :: Eval Object
tupleClass = do
   dict <- attributes
   base <- objectBase
   newType [string "tuple", base, dict]

getTupleElements :: Object -> [Object]
getTupleElements (Tuple { object_tuple = objs }) = objs
getTupleElements _other = error "bases of object is not a tuple"

attributes :: Eval Object
attributes = mkAttributesList
   [ (specialEqName, eq)
   , (specialStrName, str)
   ]

eq :: Eval Object
eq = error "== on tuple not defined"

str :: Eval Object
str = primitive 1 fun
   where
   fun (x:_) = do
      strings <- mapM showObject $ object_tuple x
      case strings of
         [oneString] -> return $ string $ "(" ++ oneString ++ ",)"
         _other -> return $ string $ "(" ++ concat (intersperse ", " strings) ++ ")"
   fun _other = error "str method on tuple applied to wrong number of arguments"
