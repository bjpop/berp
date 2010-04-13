module Berp.Base.Start (start) where

import Berp.Base.Monad (run)
import Berp.Base.SemanticTypes (Eval, Object)
import Berp.Base.StdTypes.None (none)

start :: Eval () -> IO Object 
start comp = run (comp >> return none)
