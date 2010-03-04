module Berp.Base.Builtins (builtins) where

import Prelude hiding (print)
import Berp.Base.SemanticTypes (Eval)
import Berp.Base.Builtins.Print (print)

builtins :: Eval ()
builtins = do
   -- put built in things (functions, classes) here
   print
