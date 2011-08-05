{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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

module Repl (repl) where

import Data.Typeable (Typeable (..), mkTyConApp, mkTyCon)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad (when)
import System.IO (hSetBuffering, stdout, BufferMode (..))
import Language.Python.Version3.Parser (parseStmt)
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.AST (StatementSpan)
import Language.Haskell.Exts.Pretty
   ( prettyPrintStyleMode, defaultMode, style, Style (..), PPHsMode (..)
   , Mode (..), PPLayout (PPSemiColon))
import Language.Haskell.Exts.Build (app, qualStmt)
import Language.Haskell.Exts.Syntax (Exp)
import Language.Haskell.Interpreter (setImportsQ, as, interpret)
import Berp.Version (versionString)
import Berp.Compile.Compile (compile)
import Berp.Compile.PrimName as Prim (init)
import Berp.Compile.PySyntaxUtils (InterpreterStmt (..))
import Berp.Base.SemanticTypes (Eval, Object (None), HashTable)
import Berp.Base (runWithGlobals)
import Berp.Base.Prims (printObject)
import Monad (Repl, runRepl, getGlobalScope)
import Input (getInputLines)

repl :: IO ()
repl = do
    hSetBuffering stdout NoBuffering
    greeting
    runRepl $ do
       setImportsQ [ ("Data.IntMap", Nothing)
                   , ("Data.IORef", Nothing)
                   , ("Berp.Base", Nothing)
                   , ("Berp.Base.SemanticTypes", Nothing)
                   ]
       replLoop

greeting :: IO ()
greeting = putStrLn $ "Berp version " ++ versionString ++ ", type control-d to exit."

replLoop :: Repl ()
replLoop = do
   maybeInput <- getInputLines
   case maybeInput of
      Nothing -> return ()
      Just input -> evalInput input >> replLoop

evalInput :: String -> Repl ()
evalInput input =
   when (not $ null input) $ do
      pyStmts <- liftIO $ parseAndCheckErrors (input ++ "\n")
      when (not $ null pyStmts) $ do
         exp <- lift $ lift $ compile $ InterpreterStmt pyStmts
         let expStr = oneLinePrinter exp
         comp <- interpret expStr (as :: HashTable -> Eval Object)
         globals <- getGlobalScope
         liftIO $ runWithGlobals globals $ runAndPrint comp

runAndPrint :: (HashTable -> Eval Object) -> HashTable -> Eval Object
runAndPrint comp globals = do
   -- XXX should catch exceptions here?
   result <- comp globals
   printObjectNotNone result
   return result

oneLinePrinter :: Exp -> String
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

printObjectNotNone :: Object -> Eval ()
printObjectNotNone obj@None = return ()
printObjectNotNone object = printObject object >> liftIO (putStr "\n")

-- these Typeable instances are needed by the Hint interpret function.
instance Typeable Object where
   typeOf _ = mkTyConApp (mkTyCon "Object") []

instance Typeable (Eval Object) where
   typeOf _ = mkTyConApp (mkTyCon "Eval") [typeOf (undefined :: Object)]
