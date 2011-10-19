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

module Berp.Base.StdTypes.Object (objectClass) where

import Prelude hiding (init)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import Berp.Base.Prims (primitive)
import Berp.Base.Object (identityOf)
import Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.Tuple (emptyTuple)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Bool (bool)
import Berp.Base.StdTypes.None (none)
import Berp.Base.LiftedIO as LIO (putStrLn)

objectClass :: Eval Object
objectClass = do
   LIO.putStrLn "objectClass 0"
   dict <- attributes
   LIO.putStrLn "objectClass 1"
   base <- emptyTuple
   LIO.putStrLn "objectClass 2"
   x <- newType [string "object", base, dict]
   LIO.putStrLn "objectClass 3"
   return x

attributes :: Eval Object
attributes = mkAttributesList
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
         return $ string $ "<class '" ++ typeName ++ "'>"
      Function {} -> do
         let identity = identityOf x
         return $ string $ "<function with identity " ++ show identity ++ ">"
      -- This should never happen because all other object types have a specialised
      -- str method.
      _other -> return $ string "<unknown object>"
str _other = error "str conversion on object applied to wrong number of arguments"
