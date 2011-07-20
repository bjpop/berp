-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Builtins
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A common export point for the builtin functions.
--
-----------------------------------------------------------------------------

module Berp.Base.Builtins
   ( module Exceptions, module Functions, module Constants, initBuiltins )
   where

import Berp.Base.SemanticTypes (Eval, Object, HashTable)
import Berp.Base.Hash (hashedStr)
import Berp.Base.Prims (writeGlobal)
import Berp.Base.Builtins.Constants as Constants
   (object, _s_object, _s_type, _s_bool, _s_set)
import Berp.Base.Builtins.Functions as Functions
   (print, dir, input, id, callCC)
import Berp.Base.Builtins.Exceptions as Exceptions
   ( baseException, _s_BaseException
   , exception, _s_Exception
   , stopIteration, _s_StopIteration
   , typeError, _s_TypeError
   , nameError, _s_NameError
   , valueError, _s_ValueError
   , arithmeticError, _s_ArithmeticError
   , zeroDivisionError, _s_ZeroDivisionError
   , runtimeError, _s_RuntimeError
   , notImplementedError, _s_NotImplementedError
   )

initBuiltins :: HashTable -> Eval ()
initBuiltins globalScope = do
   defineBuiltin "print" Functions.print
   defineBuiltin "callCC" Functions.callCC
   defineBuiltin "object" Constants.object
   defineBuiltin "dir" Functions.dir
   defineBuiltin "id" Functions.id
   defineBuiltin "input" Functions.input
   where
   defineBuiltin :: String -> Object -> Eval ()
   defineBuiltin name obj = do
      _ <- writeGlobal globalScope (hashedStr name) obj
      return ()
