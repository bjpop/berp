module Berp.Interpreter.Interpreter (interpreter) where

import MonadUtils
import HscTypes (liftGhcT)
import GHC
   ( defaultErrorHandler, getSessionDynFlags, setSessionDynFlags
   , findModule, mkModuleName, setContext, Ghc, SingleStep (RunToCompletion)
   , runStmt, GhcT (..), runGhcT)
import GHC.Paths ( libdir )
import DynFlags ( defaultDynFlags )
import IO (hSetBuffering, stdout, BufferMode (..))
import System.Console.Haskeline (InputT (..), runInputT)
import System.Console.Haskeline.IO
import System.Console.Haskeline as Haskeline (defaultSettings, getInputLine)
import Language.Python.Version3.Parser (parseStmt)
import Language.Python.Common.AST (StatementSpan)
import Language.Haskell.Exts.Pretty 
   ( prettyPrintStyleMode, defaultMode, style, Style (..), PPHsMode (..)
   , Mode (..), PPLayout (PPSemiColon))
import Language.Haskell.Exts.Build (doE, app, paren, qualStmt) 
import Language.Haskell.Exts.Syntax (Exp, Stmt) 
import Berp.Compile.Compile (compile)
import Berp.Compile.CompileMonad (Compile, runCompileMonad)
import Berp.Compile.PrimName as Prim (interpretStmt, init)
import Control.Monad (when)
import Berp.Compile.PySyntaxUtils (InterpreterStmt (..))
import Control.Monad.Trans (lift)

-- type Interpret a = InputT (GhcT Compile) a
-- type Interpret a = GhcT (InputT Compile) a
type Interpret a = GhcT Compile a

runInterpreter :: Maybe FilePath -> Interpret a -> IO a
runInterpreter filePath comp = 
   -- runCompileMonad $ runGhcT filePath $ runInputT defaultSettings comp
   -- runCompileMonad $ runInputT defaultSettings $ runGhcT filePath comp
   runCompileMonad $ runGhcT filePath comp
 
interpreter :: IO ()
interpreter = do
    hSetBuffering stdout NoBuffering
    inputState <- initializeInput defaultSettings
    defaultErrorHandler defaultDynFlags $ do
      runInterpreter (Just libdir) $ do
         dflags <- getSessionDynFlags
         setSessionDynFlags dflags
         -- target <- guessTarget "test_main.hs" Nothing
         -- setTargets [target]
         -- load LoadAllTargets
         prel_mod <- findModule (mkModuleName "Prelude") Nothing
         berp_base_mod <- findModule (mkModuleName "Berp.Base") Nothing
         -- setContext [] [prel_mod, berp_base_mod]
         setContext [] [berp_base_mod]
         repl inputState

repl :: InputState -> Interpret ()
repl inputState = do
   maybeInput <- liftIO $ getInputLines inputState 
   case maybeInput of 
      Nothing -> return () 
      Just input -> do
         when (not $ null input) $ do
            pyStmts <- liftIO $ parseAndCheckErrors (input ++ "\n")
            when (not $ null pyStmts) $ do
               stmts <- liftGhcT $ compile $ InterpreterStmt pyStmts
               let finalStmt = qualStmt (app Prim.interpretStmt Prim.init)
               let stmtStrs = map oneLinePrinter (stmts ++ [finalStmt])
               liftIO $ mapM_ putStrLn stmtStrs
               mapM_ (\s -> runStmt s RunToCompletion) stmtStrs
         repl inputState

getInputLines :: InputState -> IO (Maybe String)
getInputLines inputState = do
   maybeInput <- liftIO $ queryInput inputState (getInputLine ">>> ")
   case maybeInput of
      Nothing -> return Nothing
      Just line
         | null line -> return $ Just line
         | last line == ':' -> do
              continueLines <- getContinueLines inputState []
              return $ Just $ unlines (line : continueLines)
         | otherwise -> return $ Just line

getContinueLines :: InputState -> [String] -> IO [String]
getContinueLines inputState acc = do
   maybeInput <- liftIO $ queryInput inputState (getInputLine "... ")
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | null line -> return $ reverse acc
         | otherwise -> getContinueLines inputState (line:acc) 

mkHaskStmt :: [Stmt] -> Exp
mkHaskStmt = app Prim.interpretStmt . paren . doE 

oneLinePrinter :: Stmt -> String
oneLinePrinter = 
   prettyPrintStyleMode newStyle newMode
   where
   newStyle = style { mode = OneLineMode } 
   newMode = defaultMode { layout = PPSemiColon }

parseAndCheckErrors :: String -> IO [StatementSpan]
parseAndCheckErrors fileContents =
   case parseStmt fileContents "<stdin>" of
      Left e -> print e >> return [] 
      Right (pyStmt, _comments) -> return pyStmt
