module Berp.Base.StdTypes.List (list, listIndex, listClass) where

import Berp.Base.SemanticTypes (Object, ObjectRef, Eval)

list :: [Object] -> Eval Object
listClass :: Object
listIndexRef :: Object -> Object -> Eval ObjectRef
listIndex :: Object -> Object -> Eval Object
