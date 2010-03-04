module Berp.Base.Builtins.PrimFun (primFun) where

import Data.IORef (newIORef)
import Control.Monad.Trans (liftIO)
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Eval, Arity, Procedure)
import Berp.Base.Env (updateGlobalEnv)
import qualified Berp.Base.Prims as Prims (primitive)

primFun :: Ident -> Arity -> Procedure -> Eval ()
primFun ident arity proc = do
   let primitiveObj = Prims.primitive arity proc 
   ref <- liftIO $ newIORef primitiveObj 
   updateGlobalEnv ident ref
