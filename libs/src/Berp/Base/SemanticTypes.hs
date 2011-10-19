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
   , HashTable, HashSet, ListArray, Arity, ModuleCache
   , initState, Identity (..) )  where

import Control.Concurrent.MVar (MVar)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Cont (ContT)
import Data.IntMap (IntMap)
import Data.Map as Map (Map, empty)
import Data.IORef (IORef)
import Data.Complex (Complex)
import Data.Array.IO (IOArray)
import Berp.Base.Unique (Unique)

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

data EvalState =
   EvalState
   { state_control_stack :: !ControlStack
   , state_builtins :: !HashTable
   , state_moduleCache :: !ModuleCache
   , state_unique :: MVar Unique
   }

initState :: MVar Unique -> HashTable -> EvalState
initState unique builtins =
   EvalState
   { state_control_stack = EmptyStack
   , state_builtins = builtins
   , state_moduleCache = Map.empty
   , state_unique = unique
   }

type ModuleCache = Map String Object

type Eval a = StateT EvalState (ContT Object IO) a

type ObjectRef = IORef Object
type Procedure = [Object] -> Eval Object

-- XXX maybe this should be:
-- IORef (IntMap (IORef [(Object, Object)]))
-- or even:
-- IORef (IntMap (IORef [(Object, ObjectRef)]))
-- type HashTable = IORef (IntMap [(Object, Object)])

type HashTable = IORef (IntMap [(Object, ObjectRef)])
type HashSet = IORef (IntMap [Object])

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

{-
   All objects have an identity.
   Some primitive "atomic" types are self-identifying, whereas
   more complex objects have an identity created when the object
   is constructed at runtime.

   To support the id() function in Python, we need to have
   meta-identities. That is, we need an object type which
   represents identities of other objects. Objects of this
   type are "objects", and all objects have identities.

   So we need a way of satisfying:

   forall x y, id(x) == id(y) <-> x `sameObjectAs` y

   and this implies:

   forall x, id(x) != id(id(x))
-}
data Identity
   = IntegerID Integer
   | FloatID Double
   | NoneID
   | TrueID
   | FalseID
   | StringID String
   | ComplexID (Complex Double)
   | ObjectID Unique
   | IdentityID Identity
   deriving (Eq, Show)

-- XXX probably need Bound Methods.
data Object
   = Object
     { object_identity :: !Identity
     , object_type :: !Object -- type
     , object_dict :: !Object -- dictionary
     }
   | Type
     { object_identity :: !Identity
     , object_type :: Object  -- type, is this needed? Is the type of all types == type?
     , object_dict :: !Object  -- dictionary
     , object_bases :: !Object -- tuple
     , object_constructor :: !Procedure
     , object_type_name :: !Object -- string
     , object_mro :: !Object -- tuple. Method Resolution Order.
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
   | Dictionary
     { object_identity :: !Identity
     , object_hashTable :: !HashTable
     }
   | Set
     { object_identity :: !Identity
     , object_hashSet :: !HashSet
     }
   | Generator
     { object_identity :: !Identity
     , object_continuation :: !(IORef (Eval Object))
     , object_stack_context :: !(IORef (ControlStack -> ControlStack))
     }
     -- Modules probably need names and source information.
   | Module
     { object_identity :: !Identity
     , object_dict :: !Object -- dictionary
     }
   | IdentityObject { object_identity :: !Identity }
   | Integer { object_integer :: !Integer }
   | Float { object_float :: !Double }
   | Complex { object_complex :: !(Complex Double) }
   | String { object_string :: !String }
   | TrueObject
   | FalseObject
   | None

-- For debugging only
instance Show Object where
   show obj@(Object {}) = "object of (" ++ show (object_type obj) ++ ")"
   show obj@(Type {}) = "type(" ++ show (object_type_name obj) ++ ")"
   show (Tuple {}) = "tuple"
   show (List {}) = "list"
   show (Function {}) = "function"
   show (Dictionary {}) = "dictionary"
   show (Set {}) = "set"
   show (Generator {}) = "generator"
   show (Module {}) = "module"
   show (IdentityObject {}) = "identity"
   show obj@(Integer {}) = "integer(" ++ show (object_integer obj) ++ ")"
   show obj@(Float {}) = "float(" ++ show (object_float obj) ++ ")"
   show obj@(Complex {}) = "complex(" ++ show (object_complex obj) ++ ")"
   show obj@(String {}) = "string(" ++ show (object_string obj) ++ ")"
   show TrueObject = "True"
   show FalseObject = "False"
   show (None {}) = "None"
