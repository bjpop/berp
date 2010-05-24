module Berp.Interpreter.Monad (Repl, runRepl, withInputState) where

import Exception (ExceptionMonad (..))
import qualified MonadUtils as MU (MonadIO, liftIO)
import Control.Monad.Trans as MT (MonadIO (..), liftIO)
import Control.Monad.State.Strict (StateT (..), evalStateT, gets, runStateT, mapStateT) 
import GHC (GhcT (..), runGhcT)
import HscTypes (liftGhcT)
import System.Console.Haskeline as Haskeline (defaultSettings)
import System.Console.Haskeline.IO (initializeInput, InputState)
import Berp.Compile.Monad (Compile, runCompileMonad)

type Repl a = GhcT (StateT ReplState Compile) a

data ReplState = ReplState { repl_inputState :: !InputState }

runRepl :: Maybe FilePath -> Repl a -> IO a
runRepl filePath comp = do
   initInputState <- initializeInput defaultSettings
   let initReplState = ReplState { repl_inputState = initInputState }
   runCompileMonad $ (flip evalStateT) initReplState $ runGhcT filePath comp

withInputState :: (InputState -> Repl a) -> Repl a
withInputState f = do
   state <- liftGhcT $ gets repl_inputState
   f state

-- Ugliness because GHC has its own MonadIO class
instance MU.MonadIO m => MonadIO (GhcT m) where
   liftIO = MU.liftIO

instance MonadIO m => MU.MonadIO (StateT s m) where
   liftIO = MT.liftIO

instance ExceptionMonad m => ExceptionMonad (StateT s m) where
    gcatch f h = StateT $ \s -> gcatch (runStateT f s) (\e -> runStateT (h e) s)
    gblock = mapStateT gblock
    gunblock = mapStateT gunblock
