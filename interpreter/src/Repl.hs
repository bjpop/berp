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
import Language.Haskell.Exts.Syntax (Exp)
import Language.Haskell.Interpreter (setImportsQ, as, interpret)
import Berp.Version (versionString)
import Berp.Compile.Compile (compile)
import Berp.Compile.PySyntaxUtils (InterpreterStmt (..))
import Berp.Base.SemanticTypes (Eval, Object (None), HashTable)
import Berp.Base (runWithGlobals)
import Berp.Base.Prims (printObject)
import Monad (Repl, runRepl, getGlobalScope)
import Input (getInputLines)

repl :: IO ()
repl = do
    hSetBuffering stdout NoBuffering
    runRepl $ do
       setImportsQ [ ("Data.IntMap", Nothing)
                   , ("Data.IORef", Nothing)
                   , ("Berp.Base", Nothing)
                   , ("Berp.Base.SemanticTypes", Nothing)
                   ]
       greeting
       replLoop

{- This is a bit of a hack. We could, of course, just use putStrLn, to
   print the greeint. Instead we compile and run some Python code. This
   forces the GHCi to load and link all the needed modules. There is a short
   delay in the linking. We'd rather pay for the delay at the printing of
   the prompt instead of the first time the user types in an expression
   to evaluate. It seems like a small deal, but it turns out to be very
   annoying if the first expression to evaluate has a slight pause. And
   it gives users the wrong impression that the interpreter is slow, when
   in fact it is quite fast to respond.
-}
greeting :: Repl ()
greeting = evalInput $ "print(\"Berp version " ++ versionString ++ ", type control-d to exit.\")"
-- greeting = return ()

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
printObjectNotNone None = return ()
printObjectNotNone object = printObject object >> liftIO (putStr "\n")

-- these Typeable instances are needed by the Hint interpret function.
instance Typeable Object where
   typeOf _ = mkTyConApp (mkTyCon "Object") []

instance Typeable (Eval Object) where
   typeOf _ = mkTyConApp (mkTyCon "Eval") [typeOf (undefined :: Object)]
