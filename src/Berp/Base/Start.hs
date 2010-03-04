module Berp.Base.Start (start) where

import Berp.Base.Builtins (builtins)
import Berp.Base.Monad (run)
import Berp.Base.SemanticTypes (Eval)
import Berp.Base.Env (emptyVarEnv)

start :: Eval () -> IO ()
start comp = do
   env <- emptyVarEnv
   run env (builtins >> comp)
