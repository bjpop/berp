-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Builtins.Exceptions
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Standard Python exception classes.
--
-----------------------------------------------------------------------------

{-

From http://docs.python.org/dev/3.0/library/exceptions.html

BaseException
 +-- SystemExit
 +-- KeyboardInterrupt
 +-- GeneratorExit
 +-- Exception
      +-- StopIteration
      +-- ArithmeticError
      |    +-- FloatingPointError
      |    +-- OverflowError
      |    +-- ZeroDivisionError
      +-- AssertionError
      +-- AttributeError
      +-- BufferError
      +-- EnvironmentError
      |    +-- IOError
      |    +-- OSError
      |         +-- WindowsError (Windows)
      |         +-- VMSError (VMS)
      +-- EOFError
      +-- ImportError
      +-- LookupError
      |    +-- IndexError
      |    +-- KeyError
      +-- MemoryError
      +-- NameError
      |    +-- UnboundLocalError
      +-- ReferenceError
      +-- RuntimeError
      |    +-- NotImplementedError
      +-- SyntaxError
      |    +-- IndentationError
      |         +-- TabError
      +-- SystemError
      +-- TypeError
      +-- ValueError
      |    +-- UnicodeError
      |         +-- UnicodeDecodeError
      |         +-- UnicodeEncodeError
      |         +-- UnicodeTranslateError
      +-- Warning
           +-- DeprecationWarning
           +-- PendingDeprecationWarning
           +-- RuntimeWarning
           +-- SyntaxWarning
           +-- UserWarning
           +-- FutureWarning
           +-- ImportWarning
           +-- UnicodeWarning
           +-- BytesWarning
-}

module Berp.Base.Builtins.Exceptions
   ( baseException, _s_BaseException
   , exception, _s_Exception
   , stopIteration, _s_StopIteration
   , typeError, _s_TypeError
   , nameError, _s_NameError
   , lookupError, _s_LookupError
   , keyError, _s_KeyError)
   where

import Control.Monad.Trans (liftIO)
import Berp.Base.Monad (constantEval)
import Berp.Base.Builtins.Utils (primConstant)
import Berp.Base.SemanticTypes (Object (..), Eval, ObjectRef)
import Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.Object (object)
import Berp.Base.StdTypes.Tuple (tuple)
import {-# SOURCE #-} Berp.Base.StdTypes.Dictionary (dictionary)
import Berp.Base.StdTypes.String (string)

mkExceptionType :: String -> [Object] -> Eval Object
mkExceptionType name bases = do
   dict <- dictionary []
   liftIO $ newType [string name, tuple bases, dict]

baseException :: Object
baseException = constantEval $ mkExceptionType "BaseException" [object]

_s_BaseException :: ObjectRef
_s_BaseException = primConstant baseException

exception :: Object
exception = constantEval $ mkExceptionType "Exception" [baseException] 

_s_Exception :: ObjectRef
_s_Exception = primConstant exception

stopIteration :: Object
stopIteration = constantEval $ mkExceptionType "StopIteration" [exception]

_s_StopIteration :: ObjectRef
_s_StopIteration = primConstant stopIteration

typeError :: Object
typeError = constantEval $ mkExceptionType "TypeError" [exception]

_s_TypeError :: ObjectRef
_s_TypeError = primConstant typeError

nameError :: Object
nameError = constantEval $ mkExceptionType "NameError" [exception]

_s_NameError :: ObjectRef
_s_NameError = primConstant nameError

lookupError :: Object
lookupError = constantEval $ mkExceptionType "LookupError" [exception]

_s_LookupError :: ObjectRef
_s_LookupError = primConstant lookupError 

keyError :: Object
keyError = constantEval $ mkExceptionType "KeyError" [lookupError]

_s_KeyError :: ObjectRef
_s_KeyError = primConstant keyError 
