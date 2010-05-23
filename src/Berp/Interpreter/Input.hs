{-# LANGUAGE PatternGuards #-}

module Berp.Interpreter.Input (getInputLines) where

import Control.Monad (when)
import System.Console.Haskeline as Haskeline (getInputLine)
import System.Console.Haskeline.IO (queryInput)
import Berp.Interpreter.Monad (Repl, withInputState)
import Control.Monad.Trans (liftIO)
import Language.Python.Version3.Lexer (lexer, initLexState)
import Language.Python.Common.Token (Token (..))
import Language.Python.Common.ParserMonad 
   (runParser, ParseState (..), initialState)  
import Language.Python.Common.PrettyParseError ()
import Language.Python.Common.Pretty (prettyText)

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
         | Right (tokens, state) <- lexResult,
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
   isColon other = False

isEmptyLine :: [Token] -> Bool
isEmptyLine [] = False
isEmptyLine [token] = 
   isNewline token
   where
   isNewline :: Token -> Bool
   isNewline (NewlineToken {}) = True
   isNewline other = False
isEmptyLine other = False

nonEmptyParenStack :: ParseState -> Bool
nonEmptyParenStack state = not $ null $ parenStack state

nonEmptyIndentStack :: ParseState -> Bool
-- nonEmptyIndentStack state = length (indentStack state) > 1
nonEmptyIndentStack state = True 

{-
getContinueLines :: ParseState -> [String] -> Repl [String]
getContinueLines state acc = do
   maybeInput <- prompt "... " 
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | Right (tokens, newState) <- lexResult,
           nonEmptyParenStack newState || length line > 0 -> do
              -- liftIO $ print newState
              -- liftIO $ print tokens 
              getContinueLines newState (line:acc)
         | otherwise -> return $ reverse (line:acc)
         where
         lexResult = runParser lexer $ stateWithLine 
         stateWithLine = state { input = '\n':line }
-}

getIndentContinueLines :: ParseState -> [String] -> Repl [String]
getIndentContinueLines state acc = do
   maybeInput <- prompt "... " 
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | Right (tokens, newState) <- lexResult,
           nonEmptyParenStack newState -> do
              getIndentContinueLines newState (line:acc)
         | Right (tokens, newState) <- lexResult,
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
         | Right (tokens, newState) <- lexResult,
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
