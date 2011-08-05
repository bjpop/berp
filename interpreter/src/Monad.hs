-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Interpreter.Monad
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Monad type and routines for the interpreter.
--
-----------------------------------------------------------------------------

module Monad (Repl, runRepl, withInputState, getGlobalScope) where

import Control.Monad.Trans as Trans (lift, liftIO)
import Control.Monad.State.Strict as State (StateT (..), evalStateT, gets)
import Control.Monad.CatchIO as CatchIO (MonadCatchIO (..))
import Language.Haskell.Interpreter (InterpreterT, runInterpreter)
import System.Console.Haskeline as Haskeline (defaultSettings)
import System.Console.Haskeline.IO (initializeInput, InputState)
import Berp.Compile.Monad (Compile, runCompileMonad)
import Berp.Base.SemanticTypes (HashTable)
import qualified Berp.Base.HashTable as HashTable (empty)

type Repl a = InterpreterT (StateT ReplState Compile) a

data ReplState = ReplState { repl_inputState :: !InputState, repl_globalScope :: !HashTable }

runRepl :: Repl a -> IO a
runRepl comp = do
   initInputState <- initializeInput defaultSettings
   globalScope <- liftIO $ HashTable.empty
   let initReplState = ReplState { repl_inputState = initInputState, repl_globalScope = globalScope }
   result <- runCompileMonad $ (flip evalStateT) initReplState $ runInterpreter comp
   case result of
      Left e -> error $ show e
      Right val -> return val

getGlobalScope :: Repl HashTable
getGlobalScope = lift $ gets repl_globalScope

withGlobalScope :: (HashTable -> Repl a) -> Repl a
withGlobalScope f = f =<< (lift $ gets repl_globalScope)

withInputState :: (InputState -> Repl a) -> Repl a
withInputState f = f =<< (lift $ gets repl_inputState)
