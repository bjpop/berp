module Berp.Base.Builtins.Input (_s_input) where

import System.IO (stdout)
import Control.Monad (when)
import Data.List (null)
import Berp.Base.StdTypes.None (none)
import Berp.Base.SemanticTypes (Object (..), Procedure, ObjectRef, Eval)
import Berp.Base.Mangle (mangle)
import qualified Berp.Base.Prims as Prims (callMethod, printObject)
import Berp.Base.Builtins.Utils (primFun)
import Berp.Base.LiftedIO as LIO (hFlush, putStr, getLine)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)

_s_input :: ObjectRef 
_s_input = do
   primFun (mangle "input") (-1) procedure
   where
   procedure :: Procedure
   procedure objs = do
      when (not $ null objs) $ do
         printer $ head objs
         LIO.hFlush stdout
      str <- LIO.getLine
      return $ string str 
   printer :: Object -> Eval ()
   printer obj@(String {}) = LIO.putStr $ object_string obj
   printer other = Prims.printObject other

