module Berp.Base.Monad (runExpr, runStmt, interpretStmt, constantIO, constantEval) where

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Cont (runContT)
import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Berp.Base.SemanticTypes (Object (..), Eval, EvalState (..), ControlStack(EmptyStack))
import Berp.Base.Prims (printObject)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)

runExpr :: Eval Object -> IO Object 
runExpr comp 
   = runContT (evalStateT comp initState) return 
   where
   initState = EvalState { control_stack = EmptyStack } 

{-
runStmt :: Eval () -> IO Object
runStmt comp = runExpr (comp >> return none)
-}
runStmt :: Eval Object -> IO Object 
runStmt = runExpr 

interpretStmt :: Eval Object -> IO ()
interpretStmt comp = do
   runExpr $ do
      obj <- comp
      case obj of
         None {} -> return () 
         _other  -> do 
            printObject obj
            liftIO $ putStr "\n"
      return obj
   return ()

constantIO :: IO a -> a 
constantIO = unsafePerformIO

constantEval :: Eval Object -> Object 
constantEval comp = constantIO $ runContT (evalStateT comp constantState) return

constantState :: EvalState
constantState = EvalState { control_stack = EmptyStack }
