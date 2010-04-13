module Berp.Base.Builtins.Utils (primFun, primConstant) where

import Data.IORef (newIORef)
import Control.Monad.Trans (liftIO)
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Eval, Arity, Procedure, ObjectRef, Object)
import Berp.Base.Prims (primitive)
import Berp.Base.Monad (constantIO) 

primFun :: Ident -> Arity -> Procedure -> ObjectRef 
primFun ident arity proc = constantIO $ do
   let primitiveObj = primitive arity proc 
   newIORef primitiveObj 

primConstant :: Object -> ObjectRef
primConstant = constantIO . newIORef
