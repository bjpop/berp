-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Monad
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Support for the Eval monad.
--
-----------------------------------------------------------------------------

module Berp.Base.Monad (runExpr, runStmt, interpretStmt, constantIO, constantEval) where

import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Cont (runContT)
import System.IO.Unsafe (unsafePerformIO)
import Berp.Base.SemanticTypes (Object (..), Eval, EvalState (..), ControlStack(EmptyStack))
import Berp.Base.Prims (printObject)
import Berp.Base.LiftedIO as LIO (putStr)

runExpr :: Eval Object -> IO Object
runExpr comp
   = runContT (evalStateT comp initState) return
   where
   initState = EvalState { control_stack = EmptyStack }

runStmt :: Eval Object -> IO Object
runStmt = runExpr

-- This is used by the interactive interpreter to evaluate the 
-- statements entered by the user. Note that it does not print
-- None values, following the same behaviour of CPython.
interpretStmt :: Eval Object -> IO ()
interpretStmt comp = do
   _ <- runExpr $ do
      obj <- comp
      case obj of
         None {} -> return () 
         _other  -> do 
            printObject obj
            LIO.putStr "\n"
      return obj
   return ()

-- The "constant" functions below need to be used with care.
-- We try to make sure they are only used in a safe way. For the most
-- part, it is safe to use them if the IO operation is innocuous, such as
-- allocating IORefs.

constantIO :: IO a -> a 
constantIO = unsafePerformIO

constantEval :: Eval Object -> Object 
constantEval comp = constantIO $ runContT (evalStateT comp constantState) return

constantState :: EvalState
constantState = EvalState { control_stack = EmptyStack }
