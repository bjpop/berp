module Berp.Base.StdTypes.List
   (list, listIndex, listClass, updateListElement)
   where

import Berp.Base.SemanticTypes (Object, Eval)

list :: [Object] -> Eval Object
listClass :: Eval Object
listIndex :: Object -> Object -> Eval Object
updateListElement :: Object -> Object -> Object -> Eval Object
