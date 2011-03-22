-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Umbrella module for the compiler.
-- Management of the compilation process from Python to object code and
-- executable code.
--
-----------------------------------------------------------------------------

module Berp.Compile ( compilePythonToObjectFile ) where

import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST (ModuleSpan)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Cmd (system)
import System.Directory (doesFileExist, getModificationTime)
-- import System.Exit (ExitCode (..))
-- import System.FilePath ((</>), (<.>), takeBaseName)
import System.FilePath ((<.>), takeBaseName)
import System.Plugins (make, MakeStatus (..))
import Berp.Compile.Compile (compiler, patchModuleName)

-- XXX we need a way to find modules in a search path
compilePythonToObjectFile :: FilePath -> IO FilePath
compilePythonToObjectFile path = do
   fileExists <- doesFileExist path
   if not fileExists
      then error $ "Python source file not found: " ++ path
      else do
         let outputName = takeBaseName path
             mangledName = "Berp_" ++ outputName
             haskellFilename = mangledName <.> "hs"
             objectFilename = mangledName <.> "o"
         needsCompilation <- isObjectFileOutOfDate objectFilename path
         when needsCompilation $ do
            fileContents <- readFile path
            pyModule <- parseAndCheckErrors fileContents path
            haskellSrc <- prettyPrint <$> patchModuleName mangledName <$> compiler pyModule
            writeFile haskellFilename (haskellSrc ++ "\n")
            compileStatus <- make haskellFilename ["-O2", "-v0"]
            case compileStatus of
               MakeFailure errs -> error $ unlines errs
               MakeSuccess _makeCode _objectPath -> return ()
         return objectFilename

-- check if the object file is older/younger than the corresponding python file
-- Return True if the object file does not exist, or the object file is older than
-- the Python file. Older means: has a time stamp _less_ than the other file.
-- We assume we've already checked that the Python file exists.
isObjectFileOutOfDate :: FilePath -> FilePath -> IO Bool
isObjectFileOutOfDate objectPath pythonPath = do
   objExist <- doesFileExist objectPath
   if objExist
      then do
         objTime <- getModificationTime objectPath
         pyTime <- getModificationTime pythonPath
         return (objTime < pyTime)
      else return True

parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule
