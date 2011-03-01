-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.SemanticTypes
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The core types used to represent the state of Python programs. We put
-- them all here in one file because they tend to be mutually recursive.
-- Using one file for such types tends to avoid problems with unbreakable
-- cycles in the Haskell module imports. Try not to put functions in here
-- (except perhaps type class instances).
--
-----------------------------------------------------------------------------

module Berp.Base.SemanticTypes 
   ( Procedure, ControlStack (..), EvalState (..), Object (..), Eval, ObjectRef
   , HashTable, ListArray, Arity )  where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Cont (ContT) 
import Data.IntMap (IntMap)
import Data.IORef (IORef)
import Data.Complex (Complex)
import Data.Array.IO (IOArray)
import Berp.Base.Identity (Identity)

data ControlStack
   = EmptyStack 
   | ProcedureCall 
     { procedure_return :: Object -> Eval Object 
     , control_stack_tail :: ControlStack
     }
   | ExceptionHandler 
     { exception_handler :: Maybe (Object -> Eval Object)
     , exception_finally :: Maybe (Eval Object) 
     , control_stack_tail :: ControlStack
     }
   | WhileLoop 
     { loop_start :: Eval Object
     , loop_end :: Eval Object
     , control_stack_tail :: ControlStack
     }
   | GeneratorCall
     { generator_yield :: Object -> Eval Object
     , generator_object :: Object
     , control_stack_tail :: ControlStack
     } 

instance Show ControlStack where
   show EmptyStack = "EmptyStack"
   show (ProcedureCall {}) = "ProcedureCall"
   show (ExceptionHandler {}) = "ExceptionHandler"
   show (WhileLoop {}) = "WhileLoop"
   show (GeneratorCall {}) = "GeneratorCall"

data EvalState = EvalState { control_stack :: !ControlStack }

type Eval a = StateT EvalState (ContT Object IO) a

type ObjectRef = IORef Object
type Procedure = [Object] -> Eval Object

-- XXX maybe this should be:
-- IORef (IntMap (IORef [(Object, Object)]))
-- or even:
-- IORef (IntMap (IORef [(Object, ObjectRef)]))
-- type HashTable = IORef (IntMap [(Object, Object)])

type HashTable = IORef (IntMap [(Object, ObjectRef)])

type ListArray = IOArray Integer Object
type Arity = Int

{-
-- Here's another possible encoding of objects which is more abstract.
-- It would make it easier to add new object types, and they could be added
-- in separate modules. The problem is that it would make it slower for
-- detecting the kind of object we have, compared to the Alegabraic approach
-- which gives us a tag to match against.

class ObjectLike t where
   identity :: t -> Identity
   ...

data Object = forall t . (ObjectLike t, Typeable t) => O t

instance ObjectLike Object where
   identity (O x) = identity x
   ...
-}

-- XXX probably need Bound Methods.
data Object
   = Object
     { object_identity :: !Identity
     , object_type :: !Object -- type
     , object_dict :: !Object -- dictionary
     }
   | Type
     { object_identity :: !Identity
     , object_type :: Object  -- type
     , object_dict :: !Object  -- dictionary
     , object_bases :: !Object -- tuple 
     , object_constructor :: !Procedure 
     , object_type_name :: !Object -- string
     , object_mro :: !Object -- tuple. Method Resolution Order.
     }
   | Integer
     { object_identity :: !Identity
     , object_integer :: !Integer
     }
   | Float
     { object_identity :: !Identity
     , object_float :: !Double
     }
   | Complex
     { object_identity :: !Identity
     , object_complex :: Complex Double
     }
   | Bool
     { object_identity :: !Identity
     , object_bool :: !Bool
     }
   | Tuple
     { object_identity :: !Identity
     , object_tuple :: ![Object]
     , object_length :: !Int
     }
   -- We must allow the contents and the size of an array to change.
   | List
     { object_identity :: !Identity
     , object_list_elements :: !(IORef ListArray)
     , object_list_num_elements :: !(IORef Integer)
     }
   | Function
     { object_identity :: !Identity
     , object_procedure :: !Procedure
     , object_arity :: !Arity
     , object_dict :: !Object -- dictionary
     }
   | String
     { object_identity :: !Identity
     , object_string :: !String
     }
   | Dictionary
     { object_identity :: !Identity
     , object_hashTable :: !HashTable
     }
   | Generator
     { object_identity :: !Identity
     , object_continuation :: !(IORef (Eval Object))
     , object_stack_context :: !(IORef (ControlStack -> ControlStack))
     }
   | None

-- For debugging only
instance Show Object where
   show obj@(Object {}) = "object of (" ++ show (object_type obj) ++ ")"
   show obj@(Type {}) = "type(" ++ show (object_type_name obj) ++ ")"
   show obj@(Integer {}) = "integer(" ++ show (object_integer obj) ++ ")"
   show obj@(Float {}) = "float(" ++ show (object_float obj) ++ ")"
   show obj@(Bool {}) = "bool(" ++ show (object_bool obj) ++ ")"
   show (Tuple {}) = "tuple"
   show (List {}) = "list"
   show (Function {}) = "function"
   show obj@(String {}) = "string(" ++ show (object_string obj) ++ ")"
   show (Dictionary {}) = "dictionary"
   show (Generator {}) = "generator"
   show obj@(Complex {}) = "complex(" ++ show (object_complex obj) ++ ")"
   show (None {}) = "None"

-- equality instance for objects
-- NOTE: use with care. This does not call the user defined equality
-- on the object. It only uses identity equality.

instance Eq Object where
   None {} == None {} = True
   obj1 == obj2 = object_identity obj1 == object_identity obj2
