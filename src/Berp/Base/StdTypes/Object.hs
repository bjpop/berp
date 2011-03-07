-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Object
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard object type (the base of all types).
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Object (object) where

import Prelude hiding (init)
import Berp.Base.SemanticTypes (Procedure, Object (..))
import Berp.Base.Monad (constantIO)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import Berp.Base.Prims (primitive)
import Berp.Base.Object (identityOf)
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (emptyTuple)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (bool)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)

{-# NOINLINE object #-}
object :: Object
object = constantIO $ do
   dict <- attributes
   newType [string "object", emptyTuple, dict]

attributes :: IO Object
attributes = mkAttributes
   [ (specialStrName, primitive 1 str)
   , (specialEqName, primitive 2 eq)
   , (specialInitName, primitive 1 init)
   ]

-- does nothing
init :: Procedure
init _ = return none

eq :: Procedure
eq (obj1:obj2:_) = return $ bool (identityOf obj1 == identityOf obj2)
eq _other = error "equality on objects applied to wrong number of arguments"

str :: Procedure
str (x:_) = 
   case x of 
      Object {} -> do
         let objTypeNameStr = object_string $ object_type_name $ object_type x
         let identity = identityOf x 
         return $ string $ "<" ++ objTypeNameStr ++ " object with identity " ++ show identity ++ ">" 
      Type {} -> do
         let typeName = object_string $ object_type_name x
         return $ string $ "<class " ++ typeName ++ ">" 
      Function {} -> do 
         let identity = identityOf x 
         return $ string $ "<function with identity " ++ show identity ++ ">"
      -- This should never happen because all other object types have a specialised
      -- str method.
      _other -> return $ string "<unknown object>"
str _other = error "str conversion on object applied to wrong number of arguments"
