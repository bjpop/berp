module Berp.Base.StdTypes.List (list, listIndex, listClass) where

import Berp.Base.SemanticTypes (Object, Eval)

list :: [Object] -> Eval Object
listClass :: Object
listIndex :: Object -> Object -> Eval Object
