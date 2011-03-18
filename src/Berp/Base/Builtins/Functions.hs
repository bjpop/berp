-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Builtins.Functions
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Builtin functions.
--
-----------------------------------------------------------------------------

module Berp.Base.Builtins.Functions 
   (_s_print, _s_dir, _s_input, _s_id, _s_callCC)
   where

import Control.Monad (when)
import System.IO (stdout)
import Data.List (intersperse)
import Berp.Base.SemanticTypes (Object (..), Procedure, Eval, ObjectRef)
import Berp.Base.Mangle (mangle)
import qualified Berp.Base.Prims as Prims (printObject, pyCallCC)
import Berp.Base.Builtins.Utils (primFun)
import Berp.Base.LiftedIO as LIO (hFlush, hPutStr, hPutChar, getLine)
import Berp.Base.Object (dir, identityOf)
import Berp.Base.Unique (uniqueInteger)
import Berp.Base.Monad (withStdout)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Integer (int)

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
   printer obj@(String {}) = do
      withStdout $ \h -> LIO.hPutStr h $ object_string obj
   printer other = Prims.printObject other

_s_print :: ObjectRef 
_s_print = do
   primFun (mangle "print") (-1) procedure
   where
   procedure :: Procedure
   procedure objs = do
      sequence_ $ intersperse (withStdout $ \h -> LIO.hPutChar h ' ') $ map printer objs
      withStdout $ flip LIO.hPutChar '\n'
      return none
   printer :: Object -> Eval ()
   printer obj@(String {}) = withStdout $ \h -> LIO.hPutStr h $ object_string obj
   printer other = Prims.printObject other

_s_dir :: ObjectRef 
_s_dir = do
   primFun (mangle "dir") 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = dir obj
   procedure _other = error "dir applied to wrong number of arguments"

_s_id :: ObjectRef
_s_id = do
   primFun (mangle "id") 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = return $ int $ uniqueInteger $ identityOf obj
   procedure _other = error "id applied to wrong number of arguments"

_s_callCC :: ObjectRef
_s_callCC = do
   primFun (mangle "callCC") 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = Prims.pyCallCC obj 
   procedure _other = error "callCC applied to wrong number of arguments"
