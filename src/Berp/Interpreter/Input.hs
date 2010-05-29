{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Interpreter.Input
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Prompt for and read input lines. Handle input continuations for multi-line
-- statemtents. 
--
-----------------------------------------------------------------------------

module Berp.Interpreter.Input (getInputLines) where

import System.Console.Haskeline as Haskeline (getInputLine)
import System.Console.Haskeline.IO (queryInput)
import Berp.Interpreter.Monad (Repl, withInputState)
import Control.Monad.Trans (liftIO)
import Language.Python.Version3.Lexer (lexer, initLexState)
import Language.Python.Common.Token (Token (..))
import Language.Python.Common.ParserMonad 
   (runParser, ParseState (..))  
import Language.Python.Common.PrettyParseError ()

lexState :: String -> ParseState 
lexState input = initLexState input "<stdin>"

getInputLines :: Repl (Maybe String)
getInputLines = do
   maybeInput <- prompt ">>> " 
   case maybeInput of
      Nothing -> return Nothing
      Just line
         | null line -> return $ Just [] 
         | Right (tokens, state) <- lexResult,
           lastTokenIsColon tokens -> do
             restLines <- getIndentContinueLines state []
             return $ Just $ unlines (line:restLines)
         | Right (_tokens, state) <- lexResult,
           nonEmptyParenStack state -> do
             restLines <- getParenContinueLines state []
             return $ Just $ unlines (line:restLines)
         | otherwise -> return $ Just line
         where
         lexResult = runParser lexer $ lexState line

lastTokenIsColon :: [Token] -> Bool
lastTokenIsColon [] = False
lastTokenIsColon tokens = 
   isColon $ last tokens
   where
   isColon :: Token -> Bool
   isColon (ColonToken {}) = True
   isColon _other = False

nonEmptyParenStack :: ParseState -> Bool
nonEmptyParenStack state = not $ null $ parenStack state

getIndentContinueLines :: ParseState -> [String] -> Repl [String]
getIndentContinueLines state acc = do
   maybeInput <- prompt "... " 
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | Right (_tokens, newState) <- lexResult,
           nonEmptyParenStack newState -> do
              getIndentContinueLines newState (line:acc)
         | Right (_tokens, newState) <- lexResult,
           length line > 0 -> do
              -- liftIO $ print newState
              -- liftIO $ print tokens 
              getIndentContinueLines newState (line:acc)
         | otherwise -> return $ reverse (line:acc)
         where
         lexResult = runParser lexer $ stateWithLine 
         stateWithLine = state { input = '\n':line }

getParenContinueLines :: ParseState -> [String] -> Repl [String]
getParenContinueLines state acc = do
   maybeInput <- prompt "... " 
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | Right (_tokens, newState) <- lexResult,
           nonEmptyParenStack newState ->
              getParenContinueLines newState (line:acc)
         | otherwise -> return $ reverse (line:acc)
         where
         lexResult = runParser lexer $ stateWithLine 
         stateWithLine = state { input = '\n':line }

prompt :: String -> Repl (Maybe String)
prompt str = 
   withInputState prompter
   where
   prompter state = liftIO $ queryInput state $ getInputLine str
