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

module Berp.Compile ( compilePythonToHaskell ) where

import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST (ModuleSpan)
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((<.>), takeBaseName)
import Berp.Compile.Compile (compiler)

-- XXX we need a way to find modules in a search path
compilePythonToHaskell :: FilePath -> IO (FilePath, [String])
compilePythonToHaskell path = do
   fileExists <- doesFileExist path
   if not fileExists
      then error $ "Python source file not found: " ++ path
      else do
         let outputName = takeBaseName path
             mangledName = "Berp_" ++ outputName
             haskellFilename = mangledName <.> "hs"
         needsCompilation <- isFileOutOfDate haskellFilename path
         if needsCompilation
            then do
               fileContents <- readFile path
               pyModule <- parseAndCheckErrors fileContents path
               moduleMaker <- compiler pyModule
               let (haskellMod, imports) = moduleMaker mangledName
                   haskellSrc = prettyPrint haskellMod
               writeFile haskellFilename (haskellSrc ++ "\n")
               return (haskellFilename, imports)
            else return (haskellFilename, [])

-- XXX we need a way to find modules in a search path
{-
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
         needsCompilation <- isFileOutOfDate objectFilename path
         when needsCompilation $ do
            fileContents <- readFile path
            pyModule <- parseAndCheckErrors fileContents path
            -- haskellSrc <- prettyPrint <$> patchModuleName mangledName <$> compiler pyModule
            moduleMaker <- compiler pyModule
            let haskellSrc = prettyPrint $ moduleMaker mangledName
            -- haskellSrc <- prettyPrint <$> compiler pyModule
            writeFile haskellFilename (haskellSrc ++ "\n")
            -- compileStatus <- make haskellFilename ["-O2", "-v0"]
            -- compileStatus <- make haskellFilename ["-v0"]
            compileStatus <- make haskellFilename ["-fPIC"]
            case compileStatus of
               MakeFailure errs -> error $ unlines errs
               MakeSuccess _makeCode _objectPath -> return ()
         return objectFilename
-}

-- check if the file is older/younger than the corresponding python file
-- Return True if the file does not exist, or the file is older than
-- the Python file. Older means: has a time stamp _less_ than the other file.
-- We assume we've already checked that the Python file exists.
isFileOutOfDate :: FilePath -> FilePath -> IO Bool
isFileOutOfDate filePath pythonPath = do
   exists <- doesFileExist filePath
   if exists
      then do
         time <- getModificationTime filePath
         pyTime <- getModificationTime pythonPath
         return (time < pyTime)
      else return True

parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule
