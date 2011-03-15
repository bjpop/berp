-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The Main module of Berp. Both the compiler and the interactive interpreter
-- are started from here.
--
-----------------------------------------------------------------------------

module Main where

-- import Language.Python.Version3.Parser (parseModule)
-- import Language.Python.Common.AST (ModuleSpan)
import Control.Monad (when)
import Control.Applicative ((<$>))
-- import Language.Haskell.Exts.Pretty
-- import Berp.Compile.Compile (compiler, patchMainModule)
import System.Console.ParseArgs
   (Argtype (..), argDataOptional, argDataDefaulted, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import System.Cmd (system)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), (<.>), takeBaseName)
-- import System.Directory (removeFile)
-- import Berp.Interpreter.Repl (repl)
import Berp.Version (versionString)
-- import Berp.Compile.Driver (compilePythonToHaskell, compileHaskellToExe)

main :: IO ()
main = do
   let args = [withGHC, version, help, clobber, clean, showHaskell, compile, inputFile]
   argMap <- parseArgsIO ArgsComplete args
   when (gotArg argMap Help) $ do
      putStrLn $ argsUsage argMap
      exitWith ExitSuccess
   when (gotArg argMap Version) $ do
      putStrLn $ "berp version " ++ versionString
      exitWith ExitSuccess
   maybeInputDetails <- getInputDetails argMap
   case maybeInputDetails of
      -- Nothing -> repl
      Nothing -> return () -- XXX call the interpreter.
      Just (sourceName, fileContents) ->
         return () -- XXX dynamically import the module mentioned on the command line.
         -- compileAndExecute argMap sourceName fileContents

{-
compileAndExecute :: Args ArgIndex -> FilePath -> String -> IO ()
compileAndExecute argMap sourceName fileContents = do
   haskellSrc <- compilePythonToHaskell fileContents sourceName
{-
   pyModule <- parseAndCheckErrors fileContents sourceName
   -- turn this into the Main module
   haskellSrc <- prettyPrint <$> patchMainModule <$> compiler pyModule
-}
   when (gotArg argMap ShowHaskell) $ do
      putStrLn haskellSrc
      exitWith ExitSuccess

   let outputName = takeBaseName sourceName
       haskellFilename = outputName <.> "hs"
       interfaceFilename = outputName <.> "hi"
       objectFilename = outputName <.> "o"
       exeFilename = "." </> outputName
       ghcStr = maybe "ghc" id (getArg argMap WithGHC)
{-
   writeFile haskellFilename (haskellSrc ++ "\n")
   exitCodeCompile <- system $ ghcCommand argMap "-O2" "-v0" haskellFilename
-}
   exitCodeCompile <- compileHaskellToExe ghcStr haskellFilename (haskellSrc ++ "\n")
   when (gotArg argMap Compile) $ exitWith ExitSuccess
   case exitCodeCompile of
      ExitFailure {} -> exitWith exitCodeCompile
      ExitSuccess -> do
         exeStatus <- system exeFilename
         let tempFiles = [haskellFilename, interfaceFilename, objectFilename]
         when (gotArg argMap Clean) $ do
            mapM_ removeFile tempFiles
         when (gotArg argMap Clobber) $ do
            mapM_ removeFile (exeFilename : tempFiles)
         exitWith exeStatus
-}

{-
parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule
-}

getInputDetails :: Args ArgIndex -> IO (Maybe (FilePath, String))
getInputDetails argMap =
   case getArg argMap InputFile of
      Nothing -> return Nothing
      Just inputFileName -> do
         cs <- readFile inputFileName
         return $ Just (inputFileName, cs)

{-
ghcCommand :: Args ArgIndex -> String -> String -> String -> String
ghcCommand argMap optimise verbosity inputFile =
   unwords [ghcName, "--make", optimise, verbosity, inputFile]
   where
   ghcName = maybe "ghc" id (getArg argMap WithGHC)
-}

data ArgIndex
   = Help
   | ShowHaskell
   | InputFile
   | Compile
   | Clobber
   | Clean
   | WithGHC
   | Version
   deriving (Eq, Ord, Show)

help :: Arg ArgIndex
help =
   Arg
   { argIndex = Help
   , argAbbr = Just 'h'
   , argName = Just "help"
   , argData = Nothing
   , argDesc = "Display a help message."
   }

showHaskell :: Arg ArgIndex
showHaskell =
   Arg
   { argIndex = ShowHaskell
   , argAbbr = Just 't'
   , argName = Just "showhaskell"
   , argData = Nothing
   , argDesc = "Output translated Haskell code on standard output and exit."
   }

inputFile :: Arg ArgIndex
inputFile =
   Arg
   { argIndex = InputFile
   , argAbbr = Nothing
   , argName = Nothing
   , argData = argDataOptional "input file" ArgtypeString
   , argDesc = "Name of the input Python file."
   }

compile :: Arg ArgIndex
compile =
   Arg
   { argIndex = Compile
   , argAbbr = Just 'c'
   , argName = Just "compile"
   , argData = Nothing
   , argDesc = "Compile the input program, but do not run it."
   }

clobber :: Arg ArgIndex
clobber =
   Arg
   { argIndex = Clobber
   , argAbbr = Nothing
   , argName = Just "clobber"
   , argData = Nothing
   , argDesc = "Remove all compiler generated files after the compiled program has run."
   }

clean :: Arg ArgIndex
clean =
   Arg
   { argIndex = Clean
   , argAbbr = Nothing
   , argName = Just "clean"
   , argData = Nothing
   , argDesc = "Remove all compiler generated files except the executable after the compiled program has run."
   }

withGHC :: Arg ArgIndex
withGHC =
   Arg
   { argIndex = WithGHC
   , argAbbr = Nothing
   , argName = Just "with-ghc"
   , argData = argDataDefaulted "filepath to ghc" ArgtypeString "ghc"
   , argDesc = "Specify the filepath of ghc."
   }

version :: Arg ArgIndex
version =
   Arg
   { argIndex = Version
   , argAbbr = Nothing
   , argName = Just "version"
   , argData = Nothing
   , argDesc = "Show the version number of berp."
   }
