module Berp.Base.Builtins 
   ( _s_print, _s_input, _s_object, _s_BaseException, _s_Exception
   , _s_StopIteration, _s_TypeError, _s_NameError ) 
   where

import Berp.Base.SemanticTypes (ObjectRef)
import Berp.Base.Builtins.Print (_s_print)
import Berp.Base.Builtins.Input (_s_input)
import Berp.Base.Builtins.Utils (primConstant)
import Berp.Base.Builtins.Exception 
   (baseException, exception, stopIteration, typeError, nameError)
import Berp.Base.StdTypes.Object (object)

_s_object :: ObjectRef
_s_object = primConstant object

_s_BaseException :: ObjectRef
_s_BaseException = primConstant baseException

_s_Exception :: ObjectRef
_s_Exception = primConstant exception

_s_StopIteration :: ObjectRef
_s_StopIteration = primConstant stopIteration 

_s_TypeError :: ObjectRef
_s_TypeError = primConstant typeError 

_s_NameError :: ObjectRef
_s_NameError = primConstant nameError 
