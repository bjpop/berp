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

import System.FilePath (dropExtension, (<.>))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless)
import System.Console.ParseArgs
   (Argtype (..), argDataOptional, Arg (..)
   , gotArg, getArg, parseArgsIO, ArgsComplete (..), Args(..))
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.FilePath (takeBaseName)
import System.Directory (removeFile, doesFileExist)
import Berp.Version (versionString)
import qualified Data.Set as Set (Set, empty, insert, notMember)
import Berp.Compile (compilePythonToHaskell)
import System.Cmd (system)

main :: IO ()
main = do
   let args = [withGHC, version, help, clobber, clean, compile, inputFile]
   argMap <- parseArgsIO ArgsComplete args
   when (gotArg argMap Help) $ do
      putStrLn $ argsUsage argMap
      exitWith ExitSuccess
   when (gotArg argMap Version) $ do
      putStrLn $ "berp version " ++ versionString
      exitWith ExitSuccess
   maybeInputDetails <- getInputDetails argMap
   case maybeInputDetails of
      Nothing -> return ()
      Just (sourceName, _fileContents) -> do
         let exeName = pyBaseName sourceName
         exeExists <- doesFileExist exeName
         -- XXX should catch any exceptions here
         genHaskellFiles <- compilePythonFilesToHaskell Set.empty [] [sourceName]
         -- recompile if any of the source files was translated into Haskell
         -- or if the exe does not exist.
         when (not (null genHaskellFiles) || not exeExists) $ do
            writeFile "Main.hs" $ mkMainFunction $ mkHaskellModName sourceName
            ghc <- getGHC argMap
            compileStatus <- system $ ghc ++ " --make -O2 -v0 Main.hs -o " ++ exeName
            case compileStatus of
               ExitFailure _code -> exitWith compileStatus
               ExitSuccess -> return ()
         let intermediates = intermediateFiles ("Main.hs":genHaskellFiles)
         when (gotArg argMap Clean || gotArg argMap Clobber) $
            removeFiles intermediates
         unless (gotArg argMap Compile) $ do
            runStatus <- system $ "./" ++ exeName
            -- we only make it possible to remove the exe if the --compile flag was not given
            -- it makes no sense to compile to exe (without running it) and then remove the exe.
            when (gotArg argMap Clobber) $
               removeFiles [exeName]
            exitWith runStatus

intermediateFiles :: [FilePath] -> [FilePath]
intermediateFiles = concatMap mkIntermediates
   where
   mkIntermediates :: FilePath -> [FilePath]
   mkIntermediates hsFile =
      [hsFile, hiFile, objFile]
      where
      base = dropExtension hsFile
      hiFile = base <.> "hi"
      objFile = base <.> "o"

removeFiles :: [FilePath] -> IO ()
removeFiles = mapM_ removeFile

getGHC :: Args ArgIndex -> IO FilePath
getGHC argMap =
   case getArg argMap WithGHC of
      Nothing -> return "ghc"
      Just pathName -> do
         ghcExists <- doesFileExist pathName
         if ghcExists
            then return pathName
            else do
               hPutStrLn stderr $ "berp: requested version of GHC does not exist: " ++ pathName
               exitFailure

-- return the list of generated Haskell files.
compilePythonFilesToHaskell :: Set.Set String -> [FilePath] -> [FilePath] -> IO [FilePath]
compilePythonFilesToHaskell _compiledPython genHaskellFiles [] = return genHaskellFiles
compilePythonFilesToHaskell compiledPython prevGenHaskellFiles (f:fs)
   | f `Set.notMember` compiledPython = do
      (imports, nextGenHaskellFiles) <- compilePythonToHaskell f
      let pyImports = map (++ ".py") imports
          genHaskellFiles = nextGenHaskellFiles ++ prevGenHaskellFiles
      compilePythonFilesToHaskell (Set.insert f compiledPython) genHaskellFiles (fs ++ pyImports)
   | otherwise = compilePythonFilesToHaskell compiledPython prevGenHaskellFiles fs

getInputDetails :: Args ArgIndex -> IO (Maybe (FilePath, String))
getInputDetails argMap =
   case getArg argMap InputFile of
      Nothing -> return Nothing
      Just inputFileName -> do
         cs <- readFile inputFileName
         return $ Just (inputFileName, cs)

pyBaseName :: String -> String
pyBaseName pyFileName = takeBaseName pyFileName

mkHaskellModName :: String -> String
mkHaskellModName pyFileName = "Berp_" ++ pyBaseName pyFileName

mkMainFunction :: String -> String
mkMainFunction modName = unlines
   [ "module Main where"
   , "import Prelude ()"
   , "import Berp.Base (run)"
   , "import " ++ modName ++ " (init)"
   , "main = run init" ]

data ArgIndex
   = Help
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
   , argData = argDataOptional "filepath to ghc" ArgtypeString
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
