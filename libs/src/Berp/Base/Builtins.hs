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
   ( {- module Exceptions , -} module Functions, initBuiltins )
   where

import Berp.Base.SemanticTypes (Eval, Object, HashTable)
import Berp.Base.Hash (hashedStr)
import Berp.Base.Prims (writeGlobal)
import Berp.Base.Builtins.Functions as Functions
   (print, dir, input, callCC)
{-
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
-}

import Berp.Base.LiftedIO as LIO (putStrLn)
import qualified Berp.Base.StdTypes.String as String (stringClass)
import qualified Berp.Base.StdTypes.Bool as Bool (boolClass)
import qualified Berp.Base.StdTypes.Set as Set (setClass)
import qualified Berp.Base.StdTypes.Type as Type (typeClass)
import qualified Berp.Base.StdTypes.Object as Object (objectClass)
import qualified Berp.Base.StdTypes.Id as Id (idClass)
import qualified Berp.Base.StdTypes.Tuple as Tuple (tupleClass)
import qualified Berp.Base.StdTypes.Dictionary as Dictionary (dictionaryClass)
import qualified Berp.Base.StdTypes.Function as Function (functionClass)
import qualified Berp.Base.StdTypes.Module as Module (moduleClass)
import qualified Berp.Base.StdTypes.Generator as Generator (generatorClass)
import qualified Berp.Base.StdTypes.Integer as Integer (intClass)
import qualified Berp.Base.StdTypes.Complex as Complex (complexClass)
import qualified Berp.Base.StdTypes.List as List (listClass)

-- The order that the builtins are defined is important.
-- Some builtins depend on others already being defined.
initBuiltins :: HashTable -> Eval ()
initBuiltins globalScope = do
   -- types
   LIO.putStrLn "defining object"
   defineBuiltinEval "object" Object.objectClass
   LIO.putStrLn "defining type"
   defineBuiltinEval "type" Type.typeClass
   LIO.putStrLn "defining id"
   defineBuiltinEval "id" Id.idClass
   defineBuiltinEval "set" Set.setClass
   defineBuiltinEval "bool" Bool.boolClass
   defineBuiltinEval "str" String.stringClass
   defineBuiltinEval "tuple" Tuple.tupleClass
   defineBuiltinEval "dict" Dictionary.dictionaryClass
   defineBuiltinEval "function" Function.functionClass
   defineBuiltinEval "module" Module.moduleClass
   defineBuiltinEval "generator" Generator.generatorClass
   defineBuiltinEval "int" Integer.intClass
   defineBuiltinEval "complex" Complex.complexClass
   defineBuiltinEval "list" List.listClass
   -- functions
   defineBuiltinEval "print" Functions.print
   defineBuiltinEval "callCC" Functions.callCC
   defineBuiltinEval "dir" Functions.dir
   defineBuiltinEval "input" Functions.input
   where
   defineBuiltin :: String -> Object -> Eval ()
   defineBuiltin name obj = do
      _ <- writeGlobal globalScope (hashedStr name) obj
      return ()
   defineBuiltinEval :: String -> Eval Object -> Eval ()
   defineBuiltinEval name comp =
      defineBuiltin name =<< comp
