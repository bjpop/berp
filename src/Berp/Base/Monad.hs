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

module Berp.Base.Monad (runEval, interpretStmt, constantIO, constantEval, withStdout) where

import System.IO (Handle)
import Control.Monad.State.Strict (evalStateT, gets)
import Control.Monad.Cont (runContT)
import System.IO.Unsafe (unsafePerformIO)
import Berp.Base.SemanticTypes (Object (..), Eval, EvalState (..), ControlStack(EmptyStack))
-- import Berp.Base.Prims (printObject)
import Berp.Base.LiftedIO as LIO (putStr)

runEval :: Handle -> Handle -> Handle -> Eval Object -> IO Object
runEval stdin stdout stderr comp =
   runContT (evalStateT comp state) return
   where
   state = initState stdin stdout stderr

initState :: Handle -> Handle -> Handle -> EvalState
initState stdin stdout stderr =
   EvalState
   { control_stack = EmptyStack
   , state_stdin = stdin
   , state_stdout = stdout
   , state_stderr = stderr
   }

withStdout :: (Handle -> Eval a) -> Eval a
withStdout f = f =<< getStdout

getStdout :: Eval Handle
getStdout = gets state_stdout

-- This is used by the interactive interpreter to evaluate the 
-- statements entered by the user. Note that it does not print
-- None values, following the same behaviour of CPython.
interpretStmt :: Eval Object -> IO ()
interpretStmt comp = undefined
{-
   _ <- runExpr $ do
      obj <- comp
      case obj of
         None {} -> return () 
         _other  -> do 
            printObject obj
            LIO.putStr "\n"
      return obj
   return ()
-}

-- The "constant" functions below need to be used with care.
-- We try to make sure they are only used in a safe way. For the most
-- part, it is safe to use them if the IO operation is innocuous, such as
-- allocating IORefs.
-- XXX I think we should try to get rid of these. They might interact badly
-- with dynamic linking and threads.

constantIO :: IO a -> a
constantIO = unsafePerformIO

constantEval :: Eval Object -> Object
constantEval comp = constantIO $ runContT (evalStateT comp constantState) return

-- Use with extra care
constantState :: EvalState
constantState =
   EvalState
   { control_stack = EmptyStack
   , state_stdin = error "stdin not defined for constant state"
   , state_stdout = error "stdout not defined for constant state"
   , state_stderr = error "stderr not defined for constant state"
   }
