module Berp.Base.Procedure (def, lambda) where

import Control.Monad.Trans (liftIO) -- XXX can we get rid of this import?
-- import Control.Monad.State (gets)
import Data.IORef    -- XXX can we get rid of this import?
import Berp.Base.SemanticTypes (Object (..), ObjectRef, Procedure, VarEnv, Eval, EvalState(..), Arity)
import Berp.Base.StdTypes.Function (function)

-- XXX fixme. We need to handle the global scope properly. Note that the global scope in which a
-- function is defined can be different to the global scope in which it is called. This would
-- occur if the function was defined in one module and called in another. So a function needs
-- to be closed over its global scope. The question is how to do this efficiently and in a
-- way which works correctly with all the control flow subtleties. For now we will ignore the
-- issue and get back to it later on.
def :: ObjectRef -> Arity -> ([ObjectRef] -> Eval ()) -> Eval () 
def ident arity fun = do
   -- globals <- gets global_env
   -- let procedureObj = function (closure globals)
   let procedureObj = function arity closure
   liftIO $ writeIORef ident procedureObj
   where
   -- XXX Need to do an argument count check somewhere
   -- closure :: VarEnv -> Procedure 
   closure :: Procedure 
   -- closure globals params = do
   closure params = do
      argsRefs <- liftIO $ mapM newIORef params 
      -- local (\activation -> activation { global_env = globals }) (fun argsRefs) 
      fun argsRefs 
      -- may not get here, but if you do, return None
      -- XXX revisit when considering tail call optimisation
      Prelude.return None 

-- XXX fixme as above with def.
lambda :: Arity -> ([ObjectRef] -> Eval Object) -> Eval Object
lambda arity fun = do
   -- globals <- asks global_env
   -- return $ function (closure globals)
   return $ function arity closure 
   where
   -- closure :: VarEnv -> Procedure 
   closure :: Procedure 
   -- closure globals params = do
   closure params = do
      argsRefs <- liftIO $ mapM newIORef params 
      -- local (\activation -> activation { global_env = globals }) (fun argsRefs) 
      fun argsRefs
