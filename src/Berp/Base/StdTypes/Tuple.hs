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
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Object (..))
import Berp.Base.Prims (primitive, showObject)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)

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
   dict <- attributes
   newType [string "tuple", objectBase, dict]

getTupleElements :: Object -> [Object]
getTupleElements (Tuple { object_tuple = objs }) = objs
getTupleElements _other = error "bases of object is not a tuple"

attributes :: IO Object 
attributes = mkAttributes 
   [ (eqName, eq)
   , (strName, str)
   ]

eq :: Object 
eq = error "== on tuple not defined"

str :: Object 
str = primitive 1 $ \[x] -> do
   strings <- mapM showObject $ object_tuple x
   case strings of
      [oneString] -> return $ string $ "(" ++ oneString ++ ",)"
      _other -> return $ string $ "(" ++ concat (intersperse ", " strings) ++ ")"
