module Berp.Base.LiftedIO 
   ( liftIO, putStr, putStrLn, putChar, IORef, readIORef
   , writeIORef, newIORef, MonadIO, hFlush, getLine
   ) where

import Prelude hiding (putStr, putStrLn, getLine, putChar)
import qualified Prelude as P (putStr, putStrLn, getLine, putChar)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad
import Data.IORef hiding (readIORef, writeIORef, newIORef) 
import qualified Data.IORef as IORef (readIORef, writeIORef, newIORef)
import qualified System.IO as SIO (hFlush, Handle)

putStr :: MonadIO m => String -> m ()
putStr = liftIO . P.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . P.putStrLn

putChar :: MonadIO m => Char -> m ()
putChar = liftIO . P.putChar

readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . IORef.readIORef

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef x ref = liftIO $ IORef.writeIORef x ref

newIORef :: MonadIO m => a -> m (IORef a)
newIORef = liftIO . IORef.newIORef

getLine :: MonadIO m => m (String)
getLine = liftIO P.getLine

hFlush :: MonadIO m => SIO.Handle -> m ()
hFlush = liftIO . SIO.hFlush
