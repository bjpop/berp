module Berp.Base.StdTypes.Complex (complex, complexClass) where

import Data.Complex (Complex)
import Berp.Base.SemanticTypes (Object, Eval)

complex :: Complex Double -> Object
complexClass :: Eval Object
