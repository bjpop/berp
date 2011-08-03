module Berp.Base.StdTypes.Tuple 
   (tuple, tupleClass, emptyTuple, getTupleElements) where

import Berp.Base.SemanticTypes (Object)

tuple :: [Object] -> Object
tupleClass :: Object
emptyTuple :: Object
getTupleElements :: Object -> [Object]
