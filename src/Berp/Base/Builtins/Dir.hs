module Berp.Base.Builtins.Dir (_s_dir) where

import Berp.Base.SemanticTypes (Procedure, ObjectRef)
import Berp.Base.Mangle (mangle)
import Berp.Base.Builtins.Utils (primFun)
import Berp.Base.Object (dir)

_s_dir :: ObjectRef 
_s_dir = do
   primFun (mangle "dir") 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = dir obj
