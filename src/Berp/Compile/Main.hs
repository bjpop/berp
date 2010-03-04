module Main where

import Language.Python.Version3.Parser (parseModule)
import System (getArgs)
import Control.Monad (when)
import Language.Haskell.Exts.Pretty
import Berp.Compile.Compile (compiler)
import System.Console.ParseArgs
import System.Cmd
import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
   let args = [help, showHaskell, compile, inputFile]
   argMap <- parseArgsIO ArgsComplete args 
   if gotArg argMap Help 
      then error $ argsUsage argMap
      else 
         case getArg argMap InputFile of
            Nothing -> usageError argMap "No Python input file was given."
            Just filename -> do
               contents <- readFile filename
               case parseModule contents filename of
                  Left e -> print e
                  Right (pyModule, _comments) -> do
                     haskModule <- compiler pyModule 
                     let haskellSrc = prettyPrint haskModule 
                     if gotArg argMap ShowHaskell
                        then putStrLn haskellSrc
                        else do
                           writeFile "out.hs" (haskellSrc ++ "\n")
                           exitCodeCompile <- system "ghc -O2 -v0 --make out.hs"
                           if gotArg argMap Compile
                              then return ()
                              else case exitCodeCompile of
                                 ExitFailure {} -> exitWith exitCodeCompile
                                 ExitSuccess -> exitWith =<< system "./out"

data ArgIndex
   = Help 
   | ShowHaskell     
   | InputFile 
   | Compile
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
