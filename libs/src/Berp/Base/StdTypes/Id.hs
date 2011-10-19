-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Id
-- Copyright   : (c) 2011 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A type for representing object identities, implementing the id() function
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Id (idClass) where

import Prelude hiding (id)
import Berp.Base.Prims (primitive, raiseException, ret)
import Berp.Base.SemanticTypes (Object (..), Eval, Identity(..))
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.Object (identityOf)
import Berp.Base.StdNames
import Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.LiftedIO as LIO (putStrLn)

idClass :: Eval Object
idClass = do
   LIO.putStrLn "idClass 0"
   dict <- attributes
   LIO.putStrLn "idClass 1"
   base <- objectBase
   LIO.putStrLn "idClass 2"
   theType <- newType [string "id", base, dict]
   LIO.putStrLn "idClass 3"
   return $ theType { object_constructor = mkId }
   where
   mkId :: [Object] -> Eval Object
   mkId [x] = ret $ IdentityObject (IdentityID $ identityOf x)
   mkId _other = raiseException "typeError"

attributes :: Eval Object
attributes = mkAttributesList [ (specialStrName, str) ]

str :: Eval Object
str = primitive 1 fun
   where
   fun (_obj:_) = Prelude.return $ string "<Id (abstract)>"
   fun _other = error "str method on Id applied to wrong number of arguments"
