module Berp.Base.Builtins.Print (_s_print) where

import Data.List (intersperse)
import Berp.Base.StdTypes.None (none)
import Berp.Base.SemanticTypes (Object (..), Procedure, Eval, ObjectRef)
import Berp.Base.Mangle (mangle)
import qualified Berp.Base.Prims as Prims (callMethod, printObject)
import Berp.Base.Builtins.Utils (primFun)
import Berp.Base.StdNames (strName)
import Berp.Base.LiftedIO as LIO (putStr, putChar)

_s_print :: ObjectRef 
_s_print = do
   primFun (mangle "print") (-1) procedure
   where
   procedure :: Procedure
   procedure objs = do
      sequence_ $ intersperse (LIO.putChar ' ') $ map printer objs
      LIO.putChar '\n'
      return none
   printer :: Object -> Eval ()
   printer obj@(String {}) = LIO.putStr $ object_string obj
   printer other = Prims.printObject other
