module Berp.Base.Monad (run, constant) where

import Control.Monad.State.Strict (runStateT)
import Control.Monad.Cont (runContT)
import System.IO.Unsafe (unsafePerformIO)
import Berp.Base.SemanticTypes (VarEnv, Eval, EvalState (..), ControlStack(EmptyStack))
import Berp.Base.Env (emptyVarEnv)

run :: VarEnv -> Eval () -> IO () 
run env comp 
   = runContT (runStateT comp initState >> return ()) return 
   where
   initState = EvalState { global_env = env, control_stack = EmptyStack } 

constant :: IO a -> a
constant = unsafePerformIO 
