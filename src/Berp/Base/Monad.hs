module Berp.Base.Monad (run, constantIO, constantEval) where

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Cont (runContT)
import System.IO.Unsafe (unsafePerformIO)
import Berp.Base.SemanticTypes (Object, Eval, EvalState (..), ControlStack(EmptyStack))
-- import Berp.Base.Env (emptyVarEnv)

run :: Eval Object -> IO Object 
run comp 
   = runContT (evalStateT comp initState) return 
   where
   initState = EvalState { control_stack = EmptyStack } 

{-
run :: VarEnv -> Eval Object -> IO Object 
run env comp 
   = runContT (evalStateT comp initState) return 
   where
   initState = EvalState { global_env = env, control_stack = EmptyStack } 
-}

constantIO :: IO a -> a 
constantIO = unsafePerformIO

constantEval :: Eval Object -> Object 
constantEval comp = constantIO $ runContT (evalStateT comp constantState) return

{- 
class Constant t where
   constant :: t a -> a

instance Constant IO where
   constant = unsafePerformIO

instance Constant Eval where
   constant comp = constant $ runContT (runStateT comp constantState) return
-}

constantState :: EvalState
-- constantState = EvalState { global_env = undefined, control_stack = EmptyStack }
constantState = EvalState { control_stack = EmptyStack }
