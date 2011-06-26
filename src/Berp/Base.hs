-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com 
-- Stability   : experimental
-- Portability : ghc
--
-- This module exports all the primitive functions which are needed by
-- the compiled programs. Avoid putting extraneous exports in this file
-- because it is imported by all compiled programs.
--
-----------------------------------------------------------------------------

module Berp.Base
   ( module Builtins, module Complex
   , int, none, string, true, false, def, lambda, (=:), stmt, ifThenElse, ret, pass, break
   , continue, while, whileElse, for, forElse, ifThen, (@@), tailCall, tuple, read, var
   , (%), (+), (-), (*), (.), (/), (==), (<), (>), (<=), (>=), and, or, not, klass, setattr, list, dictionary
   , subs, try, tryElse, tryFinally, tryElseFinally, except, exceptDefault, raise, reRaise, raiseFrom
   , pure, pureObject, yield, generator, returnGenerator, unaryMinus, unaryPlus, invert, runEval
   , interpretStmt, topVar, unpack, setitem, Pat (..), complex, set, mkModule
   , importModule, importModuleRef, readGlobal, writeGlobal, readLocal, writeLocal )
   where

import Prelude ()
import Data.Complex as Complex (Complex (..))
import Berp.Base.Builtins as Builtins
import Berp.Base.Prims
   ( (=:), stmt, ifThenElse, ret, pass, break, continue, while, whileElse, for, forElse, ifThen,
     (@@), tailCall, read, var, setattr, subs, try, tryElse, tryFinally, tryElseFinally, except,
     exceptDefault, raise, reRaise, raiseFrom, yield, def, lambda, generator, returnGenerator, topVar, pure,
     pureObject, unpack, setitem, Pat (..), importModule, importModuleRef, readGlobal, writeGlobal,
     readLocal, writeLocal )
import Berp.Base.Operators
   ((%), (+), (-), (*), (.), (/), (==), (<), (>), (<=), (>=), and, or, unaryMinus, unaryPlus, invert, not)
import Berp.Base.Monad (runEval, interpretStmt)
import Berp.Base.Class (klass)
import Berp.Base.StdTypes.Integer (int)
import Berp.Base.StdTypes.Tuple (tuple)
import Berp.Base.StdTypes.Bool (true, false)
import Berp.Base.StdTypes.String (string)
import Berp.Base.StdTypes.None (none)
import Berp.Base.StdTypes.List (list)
import Berp.Base.StdTypes.Dictionary (dictionary)
import Berp.Base.StdTypes.Set (set)
import Berp.Base.StdTypes.Complex (complex)
import Berp.Base.StdTypes.Module (mkModule)
