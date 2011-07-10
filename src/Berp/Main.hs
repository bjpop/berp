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

import Control.Monad (when)
import System.Console.ParseArgs
   (Argtype (..), argDataOptional, argDataDefaulted, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeBaseName)
import Berp.Version (versionString)
import qualified Data.Set as Set (Set, empty, insert, notMember)
import Berp.Compile (compilePythonToHaskell)
import System.Cmd (system)

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
      Nothing -> return () -- XXX call the interpreter.
      Just (sourceName, _fileContents) -> do
         compilePythonFilesToHaskell Set.empty [sourceName]
         writeFile "Main.hs" (mkMainFunction $ mkHaskellModName sourceName)
         -- XXX check exit codes
         _ <- system "ghc --make -O2 Main.hs"
         _ <- system "./Main"
         return ()

compilePythonFilesToHaskell :: Set.Set String -> [FilePath] -> IO ()
compilePythonFilesToHaskell _previous [] = return ()
compilePythonFilesToHaskell previous (f:fs)
   | f `Set.notMember` previous = do
      (_haskellFile, imports) <- compilePythonToHaskell f
      let pyImports = map (++ ".py") imports
      compilePythonFilesToHaskell (Set.insert f previous) (fs ++ pyImports)
   | otherwise = compilePythonFilesToHaskell previous fs

getInputDetails :: Args ArgIndex -> IO (Maybe (FilePath, String))
getInputDetails argMap =
   case getArg argMap InputFile of
      Nothing -> return Nothing
      Just inputFileName -> do
         cs <- readFile inputFileName
         return $ Just (inputFileName, cs)

mkHaskellModName :: String -> String
mkHaskellModName pyFileName = "Berp_" ++ takeBaseName pyFileName

mkMainFunction :: String -> String
mkMainFunction modName = unlines
   [ "module Main where"
   , "import Prelude ()"
   , "import Berp.Base (run)"
   , "import " ++ modName ++ " (init)"
   , "main = run init" ]

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
