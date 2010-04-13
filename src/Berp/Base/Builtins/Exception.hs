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

module Berp.Base.Builtins.Exception 
   (baseException, exception, stopIteration, typeError, nameError) 
   where

import Berp.Base.Monad (constantEval)
import Berp.Base.SemanticTypes (Object (..), Procedure, Eval)
import Berp.Base.StdTypes.Type (newType)
import Berp.Base.StdTypes.Object (object)
import Berp.Base.StdTypes.Tuple (tuple)
import Berp.Base.StdTypes.Dictionary (dictionary)
import Berp.Base.StdTypes.String (string)

mkExceptionType :: String -> [Object] -> Eval Object
mkExceptionType name bases = do
   dict <- dictionary []
   newType [string name, tuple bases, dict]

baseException :: Object
baseException = constantEval $ mkExceptionType "BaseException" [object]

exception :: Object
exception = constantEval $ mkExceptionType "Exception" [baseException] 

stopIteration :: Object
stopIteration = constantEval $ mkExceptionType "StopIteration" [exception]

typeError :: Object
typeError = constantEval $ mkExceptionType "TypeError" [exception]

nameError :: Object
nameError = constantEval $ mkExceptionType "NameError" [exception]
