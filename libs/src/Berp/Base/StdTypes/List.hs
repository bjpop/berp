-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdTypes.List
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The standard list type.
--
-----------------------------------------------------------------------------

module Berp.Base.StdTypes.List (list, listClass, listIndex, updateListElement) where

import Control.Monad.Trans (liftIO)
import Berp.Base.LiftedIO (newIORef, readIORef, writeIORef)
import Data.Array.MArray (newListArray, readArray, getElems, getBounds, writeArray, newArray_)
import Data.List (intersperse)
import Data.Foldable (traverse_)
import Berp.Base.Prims (primitive, yield, pass, showObject)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval, ListArray)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributesList)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.ObjectBase (objectBase)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.Generator (generator)
import Berp.Base.StdTypes.None (none)

list :: [Object] -> Eval Object
list = liftIO . listIO

listIO :: [Object] -> IO Object
listIO elements = do
   let numElements = fromIntegral (length elements)
   array <- newListArray (0, numElements - 1) elements
   listFromArray numElements array

listFromArray :: Integer -> ListArray -> IO Object
listFromArray numElements array = do
   identity <- newIdentity
   arrayRef <- newIORef array
   sizeRef <- newIORef numElements
   return $
      List
      { object_identity = identity
      , object_list_elements = arrayRef
      , object_list_num_elements = sizeRef
      }

listIndex :: Object -> Object -> Eval Object
listIndex list index = liftIO $ do
   numElements <- readIORef $ object_list_num_elements list
   normIndex <- normaliseIndex index numElements
   array <- readIORef $ object_list_elements list
   readArray array normIndex

normaliseIndex :: Object -> Integer -> IO Integer
normaliseIndex index numElements =
   case index of
      Integer {} -> do
         let indexInteger = positiveIndex $ object_integer index
         if indexInteger < 0 || indexInteger >= numElements
            then fail "list index out of range"
            else return indexInteger
      _other -> fail "list indices must be integers"
   where
   positiveIndex index
      | index < 0 = numElements + index
      | otherwise = index

-- XXX this takes time proportional to length of first argument.
-- Can we do better than this?
listAppendItem :: Object -> Object -> Eval Object
listAppendItem list item = liftIO $ do
   let arrayRef = object_list_elements list
   array <- readIORef arrayRef
   let sizeRef = object_list_num_elements list
   oldSize <- readIORef sizeRef
   -- in future versions the bounds of the array and the number of elements could be different.
   (_lo, hi) <- getBounds array
   let newUpperBound = hi + 1
   resultArray <- newArray_ (0, newUpperBound)
   copyElements oldSize array 0 resultArray
   writeArray resultArray oldSize item
   writeIORef arrayRef resultArray
   writeIORef sizeRef $! oldSize + 1
   -- it seems that python returns none even though it might be useful to
   -- have the list returned, so you could chain appends together like so:
   -- x.append(1).append(2)
   return none

listConcat :: Object -> Object -> Eval Object
listConcat list1 list2 = liftIO $ do
   array1 <- readIORef $ object_list_elements list1
   array2 <- readIORef $ object_list_elements list2
   (_lo1, hi1) <- getBounds array1
   (_lo2, hi2) <- getBounds array2
   if hi2 < 0
      -- list2 is empty
      then return list1
      else do
         let newUpperBound = hi1 + hi2 + 1
         let size1 = hi1 + 1
         resultArray <- newArray_ (0, newUpperBound)
         copyElements size1 array1 0 resultArray
         copyElements (hi2 + 1) array2 size1 resultArray
         listFromArray (newUpperBound + 1) resultArray

copyElements :: Integer -> ListArray -> Integer -> ListArray -> IO ()
copyElements howMany from toIndex to
   = copyElementsW 0 toIndex
   where
   copyElementsW :: Integer -> Integer -> IO ()
   copyElementsW fromIndex toIndex
      | fromIndex == howMany = return ()
      | otherwise = do
           fromVal <- readArray from fromIndex
           writeArray to toIndex fromVal
           copyElementsW (fromIndex + 1) (toIndex + 1)

updateListElement :: Object -> Object -> Object -> Eval Object
updateListElement list index value = liftIO $ do
   numElements <- readIORef $ object_list_num_elements list
   normIndex <- normaliseIndex index numElements
   array <- readIORef $ object_list_elements list
   writeArray array normIndex value
   return list

{-# NOINLINE listClass #-}
listClass :: Object
listClass = constantIO $ do
   dict <- attributes
   newType [string "list", objectBase, dict]

attributes :: IO Object
attributes = mkAttributesList
   [ (specialEqName, eq)
   , (specialStrName, primitive 1 str)
   , (specialGetItemName, primitive 2 getItem)
   , (specialAddName, primitive 2 add)
   , (specialSetItemName, primitive 3 setItem)
   , (specialIterName, primitive 1 iter)
   , (appendName, primitive 2 appendItem)
   ]

eq :: Object
eq = error "== on list not defined"

getItem :: Procedure
getItem (x:y:_) = listIndex x y
getItem _other = error "getItem on list applied to wrong number of arguments"

str :: Procedure
str (x:_) = do
   elements <- liftIO $ do
      array <- readIORef $ object_list_elements x
      getElems array
   strings <- mapM showObject elements 
   -- let strings = map object_string objStrs
   Prelude.return $ string $ "[" ++ concat (intersperse ", " strings) ++ "]"
str _other = error "str on list applied to wrong number of arguments"

add :: Procedure 
add (x:y:_) = listConcat x y
add _other = error "add on list applied to wrong number of arguments"

appendItem :: Procedure
appendItem (x:y:_) = listAppendItem x y
appendItem _other = error "append on list applied to wrong number of arguments"

setItem :: Procedure 
setItem (x:y:z:_) = updateListElement x y z
setItem _other = error "setItem on list applied to wrong number of arguments"

iter :: Procedure
iter (x:_) = do
   array <- readIORef $ object_list_elements x
   elements <- liftIO $ getElems array
   generator (traverse_ yield elements >> pass)
iter _other = error "iter on list applied to wrong number of arguments"
