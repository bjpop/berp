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

import Data.List (delete, foldl')
import Control.Monad.Trans (liftIO)
import Berp.Base.SemanticTypes (Object (..), Eval, Procedure)
import Berp.Base.Monad (constantIO)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.Hash (hashedStr)
import Berp.Base.Object (typeOf)
import Berp.Base.Prims (primitive, callMethod, returningProcedure)
import Berp.Base.StdNames (mroName, initName)
import {-# SOURCE #-} Berp.Base.StdTypes.Object (object)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (emptyDictionary)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (tuple, emptyTuple)

{-# NOINLINE typeClass #-}
typeClass :: Object
typeClass = constantIO $ do 
   identity <- newIdentity
   dict <- attributes
   return $ 
      Type 
      { object_identity = identity 
      , object_type = typeClass  -- yes it is recursive!
      , object_dict = dict
      , object_bases = objectBase 
      , object_constructor = returningProcedure (\args -> liftIO $ newType args)
      , object_type_name = string "type"
      , object_mro = tuple [typeClass, object]
      }

newType :: [Object] -> IO Object 
newType args
   | [obj] <- args = return $ typeOf obj 
   | [name, bases, dict] <- args = do
        identity <- newIdentity
        let theType =
             Type 
             { object_identity = identity
             , object_type = typeClass
             , object_dict = dict
             , object_bases = bases
             , object_constructor = returningProcedure $ instantiate theType 
             , object_type_name = name 

             -- XXX we should force the eval of the mro here to catch any errors up front.
             , object_mro = tuple $ mro theType $ getTupleElements bases 
             }  
        return theType 
   | otherwise = fail "type() takes 1 or 3 arguments"

getTupleElements :: Object -> [Object] 
getTupleElements (Tuple { object_tuple = objs }) = objs
getTupleElements other = error "bases of object is not a tuple"

instantiate :: Object -> Procedure
instantiate objectType args = do
   identity <- liftIO $ newIdentity
   dict <- liftIO $ emptyDictionary
   let object =
         Object
         { object_identity = identity
         , object_type = objectType 
         , object_dict = dict
         }
   -- callMethodMaybe object initName []
   -- everything should have an init??
   callMethod object initName args 
   return object

attributes :: IO Object
attributes = 
   mkAttributes [ (mroName, primitive 1 mroMethod) ]

mroMethod :: Procedure
mroMethod (obj:_) = return $ object_mro obj 

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
   getMro other = error "Fatal error: object's base is not a type" -- XXX fixme

{-

From the Python Pep "The Python 2.3 Method Resolution Order":

  take the head of the first list, i.e L[B1][0]; if this head is not in the tail 
  of any of the other lists, then add it to the linearization of C and remove it 
  from the lists in the merge, otherwise look at the head of the next list and 
  take it, if it is a good head. Then repeat the operation until all the class 
  are removed or it is impossible to find good heads. In this case, it is 
  impossible to construct the merge, Python 2.3 will refuse to create the class 
  C and will raise an exception.

  NOTE: relies on an Eq instance for Object, which uses only identity equality.

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
        all (candidate `notElem`) (map tail seqs) ] 

   removeCandidate :: Object -> [[Object]] -> [[Object]]
   removeCandidate candidate = map (delete candidate)

   nonEmptySeqs :: [[Object]] -> [[Object]]
   nonEmptySeqs = filter (not . null) 
