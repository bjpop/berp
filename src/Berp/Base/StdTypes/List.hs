{-# OPTIONS_GHC -XTemplateHaskell #-}
module Berp.Base.StdTypes.List (list, listClass, listIndex) where

import Control.Monad.Trans (liftIO)
import Berp.Base.LiftedIO (newIORef, readIORef, writeIORef)
import Data.Array.MArray (newListArray, readArray, getElems, getBounds, writeArray, newArray_)
import Data.List (intersperse)
import Data.Foldable (traverse_)
import Berp.Base.Prims (callMethod, primitive, yield, pass, showObject)
import Berp.Base.Monad (constantIO)
import Berp.Base.SemanticTypes (Procedure, Object (..), Eval, ObjectRef, ListArray)
import Berp.Base.StdTypes.String (string)
import Berp.Base.Identity (newIdentity)
import Berp.Base.Attributes (mkAttributes)
import Berp.Base.StdNames
import {-# SOURCE #-} Berp.Base.StdTypes.Type (newType)
import {-# SOURCE #-} Berp.Base.StdTypes.ObjectBase (objectBase)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Generator (generator)

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
   return $ 
      List 
      { object_identity = identity
      , object_list_elements = arrayRef
      , object_list_num_elements = numElements
      }

listIndex :: Object -> Object -> Eval Object
listIndex list index = liftIO $ do
   let numElements = object_list_num_elements list
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
      other -> fail "list indices must be integers"
   where
   positiveIndex index
      | index < 0 = numElements + index
      | otherwise = index

listAppend :: Object -> Object -> Eval Object
listAppend list1 list2 = liftIO $ do
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
   let numElements = object_list_num_elements list
   normIndex <- normaliseIndex index numElements 
   array <- readIORef $ object_list_elements list
   writeArray array normIndex value
   return list

{-# NOINLINE listClass #-}
listClass :: Object
listClass = constantIO $ do 
   identity <- newIdentity
   dict <- attributes
   newType [string "list", objectBase, dict]
{-
   return $
      Type 
      { object_identity = identity
      , object_type = typeClass
      , object_dict = dict
      , object_bases = objectBase
      , object_constructor = \_ -> list [] 
      , object_type_name = string "list"
      }
-}

attributes :: IO Object 
attributes = mkAttributes 
   [ (eqName, eq)
   , (strName, primitive 1 str)
   , (getItemName, primitive 2 getItem) 
   , (addName, primitive 2 add)
   , (setItemName, primitive 3 setItem)
   , (iterName, primitive 1 iter)
   ]

eq :: Object 
eq = error "== on list not defined"

getItem :: Procedure 
getItem (x:y:_) = listIndex x y

str :: Procedure 
str (x:_) = do
   elements <- liftIO $ do
      array <- readIORef $ object_list_elements x  
      getElems array
   strings <- mapM showObject elements 
   -- let strings = map object_string objStrs
   Prelude.return $ string $ "[" ++ concat (intersperse ", " strings) ++ "]"

add :: Procedure 
add (x:y:_) = listAppend x y 

setItem :: Procedure 
setItem (x:y:z:_) = updateListElement x y z

iter :: Procedure
iter (x:_) = do
   array <- readIORef $ object_list_elements x
   elements <- liftIO $ getElems array
   generator (traverse_ yield elements >> pass)
