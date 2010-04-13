module Berp.Base.Builtins.Print (_s_print) where

import Control.Monad.Trans (liftIO)
import Data.List (intersperse)
import Berp.Base.StdTypes.None (none)
import Berp.Base.SemanticTypes (Object (..), Procedure, Eval, ObjectRef)
import Berp.Base.Mangle (mangle)
import qualified Berp.Base.Prims as Prims (callMethod)
import Berp.Base.Builtins.Utils (primFun)
import Berp.Base.StdNames (strName)

_s_print :: ObjectRef 
_s_print = do
   primFun (mangle "print") 1 procedure
   where
   procedure :: Procedure
   procedure objs = do
      -- should call the __str__ or maybe __repr__ method of the object
      -- liftIO $ Prelude.print [objs]
      -- XXX calling __str__ on an object should go into a library
      sequence_ $ intersperse (liftIO $ putChar ' ') $ map printObject objs
      -- mapM_ (\o -> printObject o >> liftIO (putChar ' ')) objs
      -- liftIO $ putStrLn $ "need string from object" 
      liftIO $ putChar '\n'
      return none
   -- special case for strings at the top level (no quotes around them)
   printObject (String { object_string = str }) = liftIO $ putStr str
   printObject obj = do
      -- liftIO $ putStrLn $ "Calling printObject on: " ++ show obj
      strObj <- Prims.callMethod obj strName []
      liftIO $ putStr $ object_string strObj
