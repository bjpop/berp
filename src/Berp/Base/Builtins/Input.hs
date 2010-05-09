module Berp.Base.Builtins.Input (_s_input) where

import System.IO (stdout, hFlush)
import Control.Monad (when)
import Data.List (null)
import Control.Monad.Trans (liftIO)
import Berp.Base.StdTypes.None (none)
import Berp.Base.SemanticTypes (Object (..), Procedure, ObjectRef)
import Berp.Base.Mangle (mangle)
import qualified Berp.Base.Prims as Prims (callMethod, printObject)
import Berp.Base.Builtins.Utils (primFun)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

_s_input :: ObjectRef 
_s_input = do
   primFun (mangle "input") (-1) procedure
   where
   procedure :: Procedure
   procedure objs = do
      when (not $ null objs) $ do
         Prims.printObject (head objs)
         liftIO $ hFlush stdout
      str <- liftIO $ getLine
      return $ string str 
