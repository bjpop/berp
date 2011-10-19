module Berp.Base.StdTypes.Dictionary (emptyDictionary, dictionary, dictionaryClass) where

import Berp.Base.SemanticTypes (Object, Eval)

dictionary :: [(Object, Object)] -> Eval Object
dictionaryClass :: Eval Object
emptyDictionary :: Eval Object
