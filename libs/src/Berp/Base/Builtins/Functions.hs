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
   (print, dir, input, id, callCC)
   where

import Prelude hiding (print, id)
import Control.Monad (when)
import System.IO (stdout)
import Data.List (intersperse)
import Berp.Base.SemanticTypes (Object (..), Procedure, Eval)
import qualified Berp.Base.Prims as Prims (printObject, pyCallCC, primitive)
import Berp.Base.LiftedIO as LIO (hFlush, putStr, putChar, getLine)
import qualified Berp.Base.Object as Object (dir, identityOf)
import Berp.Base.Unique (uniqueInteger)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Integer (int)
import {-# SOURCE #-} qualified Berp.Base.StdTypes.Set as Set (set)

input :: Object
input = do
   Prims.primitive (-1) procedure
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
      LIO.putStr $ object_string obj
   printer other = Prims.printObject other >> return ()

print :: Object
print =
   Prims.primitive (-1) procedure
   where
   procedure :: Procedure
   procedure objs = do
      sequence_ $ intersperse (LIO.putChar ' ') $ map printer objs
      LIO.putChar '\n'
      return none
   printer :: Object -> Eval ()
   printer obj@(String {}) = LIO.putStr $ object_string obj
   printer other = Prims.printObject other >> return ()

dir :: Object
dir = do
   Prims.primitive 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = Object.dir obj
   procedure _other = error "dir applied to wrong number of arguments"

id :: Object
id = do
   Prims.primitive 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = return $ int $ uniqueInteger $ Object.identityOf obj
   procedure _other = error "id applied to wrong number of arguments"

callCC :: Object
callCC = do
   Prims.primitive 1 procedure
   where
   procedure :: Procedure
   procedure (obj:_) = Prims.pyCallCC obj
   procedure _other = error "callCC applied to wrong number of arguments"
