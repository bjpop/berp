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

module Berp.Compile ( compilePythonToHaskell, isFileOutOfDate ) where

import Control.Applicative ((<$>), (<*>))
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST (ModuleSpan)
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((<.>), takeBaseName)
import Berp.Compile.Compile (compiler)

-- XXX we need a way to find modules in a search path
-- return True if the Haskell file was re-generated.
compilePythonToHaskell :: FilePath -> IO ([String], [FilePath])
compilePythonToHaskell path = do
   fileExists <- doesFileExist path
   if not fileExists
      then error $ "Python source file not found: " ++ path
      else do
         let outputName = takeBaseName path
             mangledName = "Berp_" ++ outputName
             haskellFilename = mangledName <.> "hs"
         fileContents <- readFile path
         pyModule <- parseAndCheckErrors fileContents path
         -- we compile regardless to collect the imports
         moduleMaker <- compiler pyModule
         let (haskellMod, imports) = moduleMaker mangledName
             haskellSrc = prettyPrint haskellMod
         -- we only save the compiled Haskell if the original was out of date
         outOfDate <- isFileOutOfDate haskellFilename path
         if outOfDate
            then do
               writeFile haskellFilename (haskellSrc ++ "\n")
               return (imports, [haskellFilename])
            else return (imports, [])

-- check if the new file is older/younger than the old file
-- Return True if the new file does not exist, or the new file is older than
-- the old file. Older means: has a time stamp _less_ than the other file.
isFileOutOfDate :: FilePath -> FilePath -> IO Bool
isFileOutOfDate newFile oldFile = do
   newExists <- doesFileExist newFile
   oldExists <- doesFileExist oldFile
   if newExists && oldExists
      then (<) <$> getModificationTime newFile <*> getModificationTime oldFile
      else return True

parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule
