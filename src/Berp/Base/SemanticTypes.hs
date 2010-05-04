module Berp.Base.SemanticTypes 
   ( Procedure, ControlStack (..), EvalState (..), Object (..), Eval, ObjectRef
   , HashTable, ListArray, Arity )  where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Cont (ContT) 
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.IORef (IORef)
import Data.Array.IO (IOArray)
import Berp.Base.Ident (Ident)
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

-- XXX can/should we make the env a dictionary/hashtable?
-- data EvalState = EvalState { global_env :: !VarEnv, control_stack :: !ControlStack }
data EvalState = EvalState { control_stack :: !ControlStack }

-- type Eval a = StateT EvalState (ContT () IO) a
type Eval a = StateT EvalState (ContT Object IO) a

-- type VarEnv = IORef (Map Ident ObjectRef)
type ObjectRef = IORef Object
type Procedure = [Object] -> Eval Object
-- XXX maybe this should be:
-- IORef (IntMap (IORef [(Object, Object)]))
-- or even:
-- IORef (IntMap (IORef [(Object, ObjectRef)]))
type HashTable = IORef (IntMap [(Object, Object)])
type ListArray = IOArray Integer Object
type Arity = Int

{-
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
     }
   | Integer
     { object_identity :: !Identity
     , object_integer :: !Integer
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
   | List 
     { object_identity :: !Identity
     , object_list_elements :: IORef ListArray
     , object_list_num_elements :: Integer
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
   show obj@(Object {}) = "object of type: " ++ show (object_type obj)
   show obj@(Type {}) = show (object_type_name obj)
   show obj@(Integer {}) = show (object_integer obj)
   show obj@(Bool {}) = show (object_bool obj)
   show (Tuple {}) = "tuple"
   show (List {}) = "list"
   show (Function {}) = "function"
   show obj@(String {}) = show (object_string obj) 
   show (None {}) = "None"
   show (Dictionary {}) = "dictionary"
   show (Generator {}) = "generator"
