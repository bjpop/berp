module Berp.Base.StdTypes.Function (function, functionClass) where

import Berp.Base.SemanticTypes (HashTable, Object (..), Procedure)

function :: Int -> Procedure -> Maybe HashTable -> Object
functionClass :: Object
