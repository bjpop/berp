module Berp.Base.StdTypes.Dictionary (emptyDict, dict, dictClass) where

import Berp.Base.SemanticTypes (Object, Eval)

dict :: [(Object, Object)] -> Eval Object
dictClass :: Object
emptyDict :: IO Object
