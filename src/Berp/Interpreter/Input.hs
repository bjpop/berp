module Berp.Interpreter.Input (getInputLines) where

import Control.Monad (when)
import System.Console.Haskeline as Haskeline (getInputLine)
import System.Console.Haskeline.IO (queryInput)
import Berp.Interpreter.Monad (Repl, withInputState)
import Control.Monad.Trans (liftIO)

getInputLines :: Repl (Maybe String)
getInputLines = do
   maybeInput <- prompt ">>> " 
   case maybeInput of
      Nothing -> return Nothing
      Just line
         | null line -> return $ Just line
         | last line == ':' -> do
              continueLines <- getContinueLines []
              return $ Just $ unlines (line : continueLines)
         | otherwise -> return $ Just line

getContinueLines :: [String] -> Repl [String]
getContinueLines acc = do
   maybeInput <- prompt "... " 
   case maybeInput of
      Nothing -> return $ reverse acc
      Just line
         | null line -> return $ reverse acc
         | otherwise -> getContinueLines (line:acc) 

prompt :: String -> Repl (Maybe String)
prompt str = 
   withInputState prompter
   where
   prompter state = 
      liftIO $ queryInput state $ getInputLine str
