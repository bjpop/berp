{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.Type
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard "type" type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.Type (typeClass, newType) where

import Berp.Base.SemanticTypes (Object (..), Procedure, Eval)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.Object (typeOf, identityOf)
import Berp.Base.Prims (primitive, callMethod, returningProcedure)
import Berp.Base.StdNames
import Berp.Base.StdTypes.Dictionary (emptyDictionary)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Tuple (tuple)
import Berp.Base.LiftedIO as LIO (putStrLn)

typeClass :: Eval Object
typeClass = do
   identity <- newIdentity
   dict <- attributes
   base <- objectBase
   let thisClass = undefined -- typeClass, XXX recursive
       objectClass = undefined -- object
   mro <- tuple [thisClass, objectClass]
   return $
      Type
      { object_identity = identity
      , object_type = undefined -- XXX fixme! yes it is recursive!
      , object_dict = dict
      , object_bases = base
      , object_constructor = returningProcedure newType
      , object_type_name = string "type"
      , object_mro = mro
      }

newType :: [Object] -> Eval Object
newType [obj] = typeOf obj
newType [name, bases, dict] = do
        identity <- newIdentity
        -- XXX we should force the eval of the mro here to catch any errors up front.
        -- mro_tuple <- tuple $ mro theType $ getTupleElements bases
        -- mro_tuple <- undefined -- XXX fixme
        mro_tuple <- error "mro_tuple" -- XXX fixme
        let theType =
             Type
             { object_identity = identity
             , object_type = undefined -- typeClass XXX fixme
             , object_dict = dict
             , object_bases = bases
             , object_constructor = returningProcedure $ instantiate theType
             , object_type_name = name
             , object_mro = mro_tuple
             }
        return theType
newType _other = fail "type() takes 1 or 3 arguments"

getTupleElements :: Object -> [Object]
getTupleElements (Tuple { object_tuple = objs }) = objs
getTupleElements _other = error "bases of object is not a tuple"

instantiate :: Object -> Procedure
instantiate objectType args = do
   identity <- newIdentity
   dict <- emptyDictionary
   let object =
         Object
         { object_identity = identity
         , object_type = objectType 
         , object_dict = dict
         }
   -- callMethodMaybe object initName []
   -- everything should have an init??
   _ <- callMethod object specialInitName args
   return object

attributes :: Eval Object
attributes =
   mkAttributesList [ (mroName, primitive 1 mroMethod) ]

mroMethod :: Procedure
mroMethod (obj:_) = return $ object_mro obj 
mroMethod _other = error "mro called with wrong number of arguments"

{- Compute the linearization of a class with respect to its base classes.

From the Python Pep "The Python 2.3 Method Resolution Order":

   "the linearization of C is the sum of C plus the merge of the 
    linearizations of the parents and the list of the parents."

    L[C(B1 ... BN)] = C + merge(L[B1] ... L[BN], B1 ... BN)

-}

mro :: Object -> [Object] -> [Object]
mro klass bases
   = klass : merge (map getMro bases ++ [bases])
   where
   getMro :: Object -> [Object]
   getMro (Type { object_mro = obj }) = getTupleElements obj
   getMro _other = error "Fatal error: object's base is not a type" -- XXX fixme

{-

From the Python Pep "The Python 2.3 Method Resolution Order":

  take the head of the first list, i.e L[B1][0]; if this head is not in the tail 
  of any of the other lists, then add it to the linearization of C and remove it 
  from the lists in the merge, otherwise look at the head of the next list and 
  take it, if it is a good head. Then repeat the operation until all the class 
  are removed or it is impossible to find good heads. In this case, it is 
  impossible to construct the merge, Python 2.3 will refuse to create the class 
  C and will raise an exception.

  The code assumes that a given class appears at most once in any sequence.
  XXX need to check this precondition. Can we check statically?
-}


merge :: [[Object]] -> [Object]
merge seqs =
   mergeWork [] $ nonEmptySeqs seqs
   where

   -- Precondition: seqs does not contain any empty sequences.

   mergeWork acc seqs
      | null seqs = reverse acc
      | candidate:_ <- findCandidate seqs
           = mergeWork (candidate:acc) $
                nonEmptySeqs $ removeCandidate candidate seqs
      | otherwise
           = error "Cannot create a consistent method resolution" -- XXX should we make this an exception?

   -- Precondition: seqs does not contain any empty sequences.
   -- Otherwise the "head" and "tail" are not safe.

   findCandidate :: [[Object]] -> [Object]
   findCandidate seqs =
      [ candidate | candidate <- map head seqs,
        all (not . (member candidate)) (map tail seqs) ]

   removeCandidate :: Object -> [[Object]] -> [[Object]]
   removeCandidate candidate = map (remove candidate)

   nonEmptySeqs :: [[Object]] -> [[Object]]
   nonEmptySeqs = filter (not . null)

   sameIdentity :: Object -> Object -> Bool
   sameIdentity obj1 obj2 = identityOf obj1 == identityOf obj2

   member :: Object -> [Object] -> Bool
   member obj = foldr test False
      where
      test :: Object -> Bool -> Bool
      test next rest = obj `sameIdentity` next || rest

   remove :: Object -> [Object] -> [Object]
   remove obj = foldr test []
      where
      test :: Object -> [Object] -> [Object]
      test next rest
         | obj `sameIdentity` next = rest
         | otherwise = next : rest
