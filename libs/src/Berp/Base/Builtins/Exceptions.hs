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
   ( {- baseException, _s_BaseException
   , exception,
   , stopIteration,
   , typeError,
   , nameError,
   , lookupError,
   , keyError,
   , valueError,
   , arithmeticError,
   , zeroDivisionError,
   , runtimeError,
   , notImplementedError,
   -}
   )
   where

-- import Berp.Base.SemanticTypes (Object (..), Eval)
-- import Berp.Base.StdTypes.Type (newType)
-- import Berp.Base.StdTypes.Tuple (tuple)
-- import Berp.Base.StdTypes.Dictionary (dictionary)
-- import Berp.Base.StdTypes.String (string)

{-
mkExceptionType :: String -> [Object] -> Eval Object
mkExceptionType name bases = do
   dict <- dictionary []
   base <- tuple bases
   newType [string name, base, dict]

baseException :: Object
baseException = constantEval $ mkExceptionType "BaseException" [object]

exception :: Object
exception = constantEval $ mkExceptionType "Exception" [baseException] 

stopIteration :: Object
stopIteration = constantEval $ mkExceptionType "StopIteration" [exception]

arithmeticError :: Object
arithmeticError = constantEval $ mkExceptionType "ArithmeticError" [exception]

zeroDivisionError :: Object
zeroDivisionError = constantEval $ mkExceptionType "ZeroDivisionError" [arithmeticError]

typeError :: Object
typeError = constantEval $ mkExceptionType "TypeError" [exception]

nameError :: Object
nameError = constantEval $ mkExceptionType "NameError" [exception]

lookupError :: Object
lookupError = constantEval $ mkExceptionType "LookupError" [exception]

keyError :: Object
keyError = constantEval $ mkExceptionType "KeyError" [lookupError]

valueError :: Object
valueError = constantEval $ mkExceptionType "ValueError" [exception]

runtimeError :: Object
runtimeError = constantEval $ mkExceptionType "RuntimeError" [exception]

notImplementedError :: Object
notImplementedError = constantEval $ mkExceptionType "NotImplementedError" [runtimeError]
-}
