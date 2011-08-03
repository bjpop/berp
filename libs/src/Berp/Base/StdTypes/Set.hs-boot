module Berp.Base.StdTypes.Set (emptySet, set, setClass) where

import Berp.Base.SemanticTypes (Object, Eval)

set :: [Object] -> Eval Object
setClass :: Object
emptySet :: IO Object
