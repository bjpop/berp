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

import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST (ModuleSpan)
import System.Environment (getArgs)
import Control.Monad (when)
import Control.Applicative ((<$>))
import Language.Haskell.Exts.Pretty
import Berp.Compile.Compile (compiler)
import System.Console.ParseArgs
import System.Cmd
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), (<.>), takeBaseName)
import System.Directory (removeFile)
import Berp.Interpreter.Repl (repl)

main :: IO ()
main = do
   let args = [help, clobber, clean, showHaskell, compile, inputFile]
   argMap <- parseArgsIO ArgsComplete args 
   giveHelp argMap
   maybeInputDetails <- getInputDetails argMap
   case maybeInputDetails of
      Nothing -> repl 
      Just (sourceName, fileContents) -> 
         compileAndExecute argMap sourceName fileContents
 
compileAndExecute :: Args ArgIndex -> FilePath -> String -> IO ()
compileAndExecute argMap sourceName fileContents = do
   pyModule <- parseAndCheckErrors fileContents sourceName
   haskellSrc <- prettyPrint <$> compiler pyModule 
   when (gotArg argMap ShowHaskell) $ do
      putStrLn haskellSrc
      exitWith ExitSuccess
   let outputName = takeBaseName sourceName 
       haskellFilename = outputName <.> "hs"
       interfaceFilename = outputName <.> "hi" 
       objectFilename = outputName <.> "o" 
       exeFilename = "." </> outputName
   writeFile haskellFilename (haskellSrc ++ "\n")
   exitCodeCompile <- system $ ghcCommand "-O2" "-v0" haskellFilename
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

parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule 

getInputDetails :: Args ArgIndex -> IO (Maybe (FilePath, String))
getInputDetails argMap = 
   case getArg argMap InputFile of
      Nothing -> return Nothing 
      Just inputFileName -> do
         cs <- readFile inputFileName
         return $ Just (inputFileName, cs)

giveHelp :: Args ArgIndex -> IO ()
giveHelp argMap =
   if gotArg argMap Help 
      then error $ argsUsage argMap
      else return ()

ghcCommand :: String -> String -> String -> String 
ghcCommand optimise verbosity inputFile
   = unwords ["ghc", "--make", optimise, verbosity, inputFile]

data ArgIndex
   = Help 
   | ShowHaskell     
   | InputFile 
   | Compile
   | Clobber 
   | Clean
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

showHaskell = 
   Arg
   { argIndex = ShowHaskell 
   , argAbbr = Just 't' 
   , argName = Just "showhaskell"
   , argData = Nothing 
   , argDesc = "Output translated Haskell code on standard output and exit."
   }

inputFile = 
   Arg
   { argIndex = InputFile 
   , argAbbr = Nothing 
   , argName = Nothing 
   , argData = argDataOptional "input file" ArgtypeString
   , argDesc = "Name of the input Python file."
   }

compile = 
   Arg
   { argIndex = Compile 
   , argAbbr = Just 'c' 
   , argName = Just "compile" 
   , argData = Nothing 
   , argDesc = "Compile the input program, but do not run it."
   }

clobber = 
   Arg
   { argIndex = Clobber 
   , argAbbr = Nothing
   , argName = Just "clobber" 
   , argData = Nothing 
   , argDesc = "Remove all compiler generated files after the compiled program has run."
   }

clean = 
   Arg
   { argIndex = Clean 
   , argAbbr = Nothing
   , argName = Just "clean" 
   , argData = Nothing 
   , argDesc = "Remove all compiler generated files except the executable after the compiled program has run."
   }
