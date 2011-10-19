module Berp.Base.StdTypes.Tuple
   (tuple, tupleClass, emptyTuple, getTupleElements) where

import Berp.Base.SemanticTypes (Object, Eval)

tuple :: [Object] -> Eval Object
tupleClass :: Eval Object
emptyTuple :: Eval Object
getTupleElements :: Object -> [Object]
