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

module Berp.Base.Monad
   ( runEval, interpretStmt, constantIO, constantEval, withStdout
   , updateModuleCache, lookupModuleCache
   ) where

import Control.Applicative ((<$>))
import System.IO (Handle)
import Control.Monad.State.Strict (evalStateT, gets, modify)
import Control.Monad.Cont (runContT)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map (lookup, insert)
import Berp.Base.SemanticTypes
   (Object (..), Eval, EvalState (..), ControlStack (EmptyStack), GlobalScope (..))
-- import Berp.Base.Prims (printObject)
import Berp.Base.LiftedIO as LIO (putStr)


runEval :: EvalState -> Eval Object -> IO Object
runEval state comp = runContT (evalStateT comp state) return

{-
initState :: Handle -> Handle -> Handle -> HashTable -> EvalState
initState stdin stdout stderr globalScope =
   EvalState
   { control_stack = EmptyStack
   , global_scope = TopGlobalScope globalScope
   , state_stdin = stdin
   , state_stdout = stdout
   , state_stderr = stderr
   , state_moduleCache = Map.empty
   }
-}

lookupModuleCache :: String -> Eval (Maybe Object)
lookupModuleCache moduleName =
   Map.lookup moduleName <$> gets state_moduleCache

-- XXX is there a lazy leak here in the update?
updateModuleCache :: String -> Object -> Eval ()
updateModuleCache moduleName object = do
   oldCache <- gets state_moduleCache
   modify $ \state -> state { state_moduleCache = Map.insert moduleName object oldCache }

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

-- XXX this needs to be removed
constantEval :: Eval Object -> Object
constantEval comp = constantIO $ runContT (evalStateT comp constantState) return

-- Use with extra care
-- XXX this needs to be removed
constantState :: EvalState
constantState =
   EvalState
   { control_stack = EmptyStack
   , state_global_scope = error "global scope not defined for constant state"
   , state_stdin = error "stdin not defined for constant state"
   , state_stdout = error "stdout not defined for constant state"
   , state_stderr = error "stderr not defined for constant state"
   , state_moduleCache = error "module cache not defined for constant state"
   }
