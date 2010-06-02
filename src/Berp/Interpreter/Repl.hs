-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Interpreter.Repl
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The Read Eval Print Loop (REPL) of the interpreter.
--
-----------------------------------------------------------------------------

module Berp.Interpreter.Repl (repl) where

import MonadUtils
import HscTypes (liftGhcT)
import Control.Monad.Trans (lift)
import GHC
   ( defaultErrorHandler, getSessionDynFlags, setSessionDynFlags
   , findModule, mkModuleName, setContext, SingleStep (RunToCompletion)
   , runStmt, gcatch, RunResult (..))
import Control.Monad (when)
import Control.Exception.Extensible (SomeException (..))
import GHC.Paths (libdir)
import DynFlags (defaultDynFlags)
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Language.Python.Version3.Parser (parseStmt)
-- import Language.Python.Common.PrettyParseError 
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.AST (StatementSpan)
import Language.Haskell.Exts.Pretty 
   ( prettyPrintStyleMode, defaultMode, style, Style (..), PPHsMode (..)
   , Mode (..), PPLayout (PPSemiColon))
import Language.Haskell.Exts.Build (app, qualStmt) 
import Language.Haskell.Exts.Syntax (Stmt) 
import Berp.Version (versionString)
import Berp.Compile.Compile (compile)
import Berp.Compile.PrimName as Prim (interpretStmt, init)
import Berp.Compile.PySyntaxUtils (InterpreterStmt (..))
import Berp.Interpreter.Monad (Repl, runRepl)
import Berp.Interpreter.Input (getInputLines)
 
repl :: IO ()
repl = do
    hSetBuffering stdout NoBuffering
    greeting
    defaultErrorHandler defaultDynFlags $ do
      runRepl (Just libdir) $ do
         dflags <- getSessionDynFlags
         setSessionDynFlags dflags
         -- target <- guessTarget "test_main.hs" Nothing
         -- setTargets [target]
         -- load LoadAllTargets
         -- prel_mod <- findModule (mkModuleName "Prelude") Nothing
         berp_base_mod <- findModule (mkModuleName "Berp.Base") Nothing
         -- setContext [] [prel_mod, berp_base_mod]
         setContext [] [berp_base_mod]
         replLoop

greeting :: IO ()
greeting = putStrLn $ "Berp version " ++ versionString ++ ", type control-d to exit."

replLoop :: Repl ()
replLoop = do
   maybeInput <- getInputLines
   case maybeInput of 
      Nothing -> return () 
      Just input -> do
         when (not $ null input) $ do
            pyStmts <- liftIO $ parseAndCheckErrors (input ++ "\n")
            when (not $ null pyStmts) $ do
               stmts <- liftGhcT $ lift $ compile $ InterpreterStmt pyStmts
               let finalStmt = qualStmt (app Prim.interpretStmt Prim.init)
               let stmtStrs = map oneLinePrinter (stmts ++ [finalStmt])
               -- liftIO $ mapM_ putStrLn stmtStrs
               mapM_ runAndCatch stmtStrs
         replLoop

runAndCatch :: String -> Repl ()
runAndCatch stmt = do 
   gcatch (runStmt stmt RunToCompletion >>= printRunResult) catcher
   where
   catcher :: SomeException -> Repl ()
   catcher e = liftIO $ print e 

printRunResult :: RunResult -> Repl ()
printRunResult (RunException e) = liftIO $ putStrLn ("Exception " ++ show e)
printRunResult _other = return () 

oneLinePrinter :: Stmt -> String
oneLinePrinter = 
   prettyPrintStyleMode newStyle newMode
   where
   newStyle = style { mode = OneLineMode } 
   newMode = defaultMode { layout = PPSemiColon }

parseAndCheckErrors :: String -> IO [StatementSpan]
parseAndCheckErrors fileContents =
   case parseStmt fileContents "<stdin>" of
      Left e -> (putStrLn $ prettyText e) >> return [] 
      Right (pyStmt, _comments) -> return pyStmt
