{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PatternGuards #-}

-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Prims
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Implementation of primitive functions.
--
-----------------------------------------------------------------------------

#include "BerpDebug.h"

module Berp.Base.Prims
   ( (=:), stmt, ifThenElse, ret, pass, break
   , continue, while, whileElse, for, forElse, ifThen, (@@), tailCall
   , read, var, binOp, setattr, callMethod, callSpecialMethod, subs
   , try, tryElse, tryFinally, tryElseFinally, except, exceptDefault
   , raise, reRaise, raiseFrom, primitive, generator, yield, generatorNext
   , def, lambda, returnGenerator, printObject, topVar, Applicative.pure
   , pureObject, showObject, returningProcedure, pyCallCC, unpack
   , next, setitem, Pat (G, V), getIterator, mapIterator
   , importModule, importModuleRef, readGlobal, writeGlobal, readLocal
   , writeLocal, getGlobalScopeHashTable ) where

import Prelude hiding (break, read, putStr)
import System.Plugins (load_, LoadStatus (..))
import Control.Monad (zipWithM)
import Control.Monad.State (gets, modify)
import Control.Monad.Cont (callCC)
import Data.Array.IO (getElems)
import Berp.Compile (compilePythonToObjectFile)
import Berp.Base.LiftedIO as LIO (readIORef, writeIORef, newIORef, hPutStr, liftIO)
#ifdef DEBUG
import Berp.Base.LiftedIO as LIO (putStrLn)
#endif
import qualified Control.Applicative as Applicative (pure)
import Control.Applicative ((<$>))
import Berp.Base.Monad (withStdout, updateModuleCache, lookupModuleCache)
import Berp.Base.SemanticTypes
   ( HashTable, Object (..), ObjectRef, Procedure, Eval, EvalState(..), ControlStack(..), Arity, GlobalScope (..) )
import Berp.Base.Truth (truth)
import {-# SOURCE #-} Berp.Base.Object
   ( typeOf, dictOf, lookupAttribute, lookupSpecialAttribute, objectEquality, isIterator )
import Berp.Base.Hash (Hashed)
import Berp.Base.ControlStack
import Berp.Base.StdNames (specialDocName, specialStrName, specialSetItemName, specialGetItemName, specialNextName, specialIterName)
import Berp.Base.Exception (RuntimeError (..), throw)
import Berp.Base.Ident (Ident)
import {-# SOURCE #-} Berp.Base.HashTable as Hash (empty, stringInsert, insert, stringLookup)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (function)
import {-# SOURCE #-} Berp.Base.StdTypes.List (updateListElement)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (true, false)
import {-# SOURCE #-} Berp.Base.StdTypes.Generator (generator)
import {-# SOURCE #-} Berp.Base.Builtins.Exceptions (stopIteration, typeError, valueError, nameError)

data Pat = G Int [Pat] | V ObjectRef

-- specialised to monomorphic type for the benefit of the interpreter.
-- otherwise we'd need to add a type annotation in the generated code.
pureObject :: Object -> Eval Object
pureObject = Applicative.pure

getGlobalScopeHashTable :: Eval HashTable
getGlobalScopeHashTable = global_scope_bindings <$> gets state_global_scope

primitive :: Arity -> Procedure -> Object
primitive arity proc = function arity (returningProcedure proc) Nothing

returningProcedure :: Procedure -> Procedure
returningProcedure proc args = do
   result <- proc args
   ret result

infix 1 =:  -- assignment
infixl 8 @@ -- procedure application

topVar :: Ident -> IO ObjectRef
topVar s = newIORef (error $ "undefined variable:" ++ s)

var :: Ident -> Eval ObjectRef
var s = newIORef (error $ "undefined variable: " ++ s)

read :: ObjectRef -> Eval Object
read = readIORef

readLocal :: ObjectRef -> Eval Object
readLocal = readIORef

writeLocal :: ObjectRef -> Object -> Eval Object
writeLocal var obj = writeIORef var obj >> return none

readGlobal :: Hashed String -> Eval Object
readGlobal var = do
   -- scope <- gets state_global_scope
   -- bindings <- readIORef $ global_scope_bindings scope
   -- let bindings = global_scope_bindings scope
   globalScope <- getGlobalScopeHashTable
   maybeObj <- stringLookup var globalScope
   case maybeObj of
      -- Nothing -> raise nameError
      Nothing -> error (snd var)
      Just obj -> return obj

writeGlobal :: Hashed String -> Object -> Eval Object
writeGlobal var obj = do
   globalScope <- getGlobalScopeHashTable
   stringInsert var obj globalScope
   return none

ret :: Object -> Eval Object
ret obj = do
   stack <- unwind isProcedureCall
   procedure_return stack obj

pass :: Eval Object
pass = return none

break :: Eval Object
break = do
   stack <- unwindPastWhileLoop 
   loop_end stack

continue :: Eval Object
continue = do 
   stack <- unwindUpToWhileLoop 
   loop_start stack

-- We return None because that works well in the interpreter. None values
-- are not printed by default, so it matches the same behaviour as the
-- CPython interpreter.
(=:) :: ObjectRef -> Object -> Eval Object
ident =: obj = writeIORef ident obj >> return none

-- XXX we could have specialised versions for certain small arities and thus
-- dispense with the list of objects
(@@) :: Object -> [Object] -> Eval Object
obj @@ args = do
    case obj of
        Function { object_procedure = proc
                 , object_arity = arity
                 , object_global_scope = globals }
           | arity == -1 || arity == length args -> do
                pushGlobalScope globals
                result <- callProcedure proc args
                popGlobalScope
                return result
           -- XXX should be raise of arity, typeError exception
           | otherwise -> raise typeError
        -- XXX not sure if this needs global scope push/pop
        Type { object_constructor = proc } -> callProcedure proc args
        -- XXX should try to find "__call__" attribute on object
        _other -> raise typeError

callProcedure :: Procedure -> [Object] -> Eval Object
callProcedure proc args =
   callCC $ \ret -> do
      push $ ProcedureCall ret
      proc args

-- need to hanfle global scope properly. Can't push/pop, that would invalidate
-- the tail call. Instead need to modify in place the top global scope.
tailCall :: Object -> [Object] -> Eval Object
tailCall obj args = do
    case obj of
        Function { object_procedure = proc
                 , object_arity = arity
                 , object_global_scope = globals }
           | arity == -1 || arity == length args -> do
                modifyGlobalScope globals
                proc args
           | otherwise -> raise typeError
        -- XXX not sure if we have to handle global scope here
        Type { object_constructor = proc } -> proc args
        -- XXX should try to find "__call__" attribute on object
        _other -> raise typeError

ifThenElse :: Eval Object -> Eval Object -> Eval Object -> Eval Object
ifThenElse condComp trueComp falseComp = do
    cond <- condComp
    if truth cond then trueComp else falseComp

ifThen :: Eval Object -> Eval Object -> Eval Object
ifThen condComp trueComp = do
   cond <- condComp
   if truth cond then trueComp else pass

{-
Compile for loops by desugaring into while loops.

   for vars in exp:
      suite1
   else:
      suite2

desugars to --->

   fresh_var_1 = exp.__iter__()
   fresh_var_2 = True
   while fresh_var_2:
      try:
         vars = fresh_var_1.__next__()
         suite1
      except StopIteration:
         fresh_var_2 = False
   else:
      suite2
-}

for :: Object -> (Object -> Eval Object) -> Eval Object
for exp body = forElse exp body pass

forElse :: Object -> (Object -> Eval Object) -> Eval Object -> Eval Object
forElse expObj suite1 suite2 = do
   iterObj <- callMethod expObj specialIterName [] -- this could be specialised
   cond <- newIORef true
   let tryBlock = do -- nextObj <- callMethod iterObj specialNextName [] -- this could be specialised
                     nextObj <- next iterObj
                     -- writeIORef var nextObj
                     suite1 nextObj
   let handler e = except e stopIteration ((writeIORef cond false) >> pass) (raise e)
   let whileBlock = try tryBlock handler
   whileElse (readIORef cond) whileBlock suite2

while :: Eval Object -> Eval Object -> Eval Object 
while cond loopBlock = whileElse cond loopBlock pass 

whileElse :: Eval Object -> Eval Object -> Eval Object -> Eval Object
whileElse cond loopBlock elseBlock = do
   callCC $ \end -> do
      let afterLoop = end none
          loop = do condVal <- cond
                    if truth condVal
                       then do
                          _ <- loopBlock
                          loop
                       -- this does the unwind before the else block,
                       -- otherwise a call to break or continue in the else block
                       -- would have undesired results
                       else do
                          _ <- unwindPastWhileLoop
                          _ <- elseBlock
                          afterLoop
      push $ WhileLoop loop afterLoop
      loop

stmt :: Eval Object -> Eval Object
-- stmt comp = comp >> pass 
-- Extra strictness needed here to ensure the value of the comp is demanded (in case exceptions are raised etc).
-- stmt comp = comp >>= (\obj -> seq obj pass)
stmt = id 

-- XXX could this be turned into a type class?
binOp :: Object -> Object -> (Object -> t) -> (t -> t -> r) -> (r -> Eval Object) -> Eval Object
binOp left right project fun build 
   = build (project left `fun` project right)

-- XXX this should also work on Type
-- XXX need to support __setattr__ and descriptors
setattr :: Object -> Hashed String -> Object -> Eval Object
setattr target attribute value 
   | Just dict <- dictOf target = do
        let hashTable = object_hashTable dict
        Hash.stringInsert attribute value $ hashTable
        return value
   | otherwise = error $ "setattr on object unimplemented: " ++ show (target, attribute)

setitem :: Object -> Object -> Object -> Eval Object
setitem (Dictionary { object_hashTable = hashTable }) index value
   = Hash.insert index value hashTable >> return none
setitem list@(List {}) index value
   = updateListElement list index value >> return none
setitem obj index value
   = callMethod obj specialSetItemName [index, value]

callMethod :: Object -> Hashed String -> [Object] -> Eval Object
callMethod object ident args = do
   proc <- lookupAttribute object ident
   proc @@ args

-- this one goes straight to the type, skipping the dictionary of the object
callSpecialMethod :: Object -> Hashed String -> [Object] -> Eval Object
callSpecialMethod object ident args = do
   proc <- lookupSpecialAttribute object ident
   proc @@ args

subs :: Object -> Object -> Eval Object
subs obj subscript = callMethod obj specialGetItemName [subscript]

try :: Eval Object -> (Object -> Eval Object) -> Eval Object
try tryComp handler = tryWorker tryComp handler pass Nothing

tryElse :: Eval Object -> (Object -> Eval Object) -> Eval Object -> Eval Object
tryElse tryComp handler elseComp =
   tryWorker tryComp handler elseComp Nothing

tryFinally :: Eval Object -> (Object -> Eval Object) -> Eval Object -> Eval Object
tryFinally tryComp handler finallyComp
   = tryWorker tryComp handler pass (Just finallyComp)

tryElseFinally :: Eval Object -> (Object -> Eval Object) -> Eval Object -> Eval Object -> Eval Object
tryElseFinally tryComp handler elseComp finallyComp
   = tryWorker tryComp handler elseComp (Just finallyComp)

tryWorker :: Eval Object -> (Object -> Eval Object) -> Eval Object -> Maybe (Eval Object) -> Eval Object
tryWorker tryComp handler elseComp maybeFinallyComp = do
   _ <- callCC $ \afterTry -> do
      push (ExceptionHandler
              (Just $ \obj -> do
                   _ <- handler obj
                   afterTry none)
              maybeFinallyComp)
      _ <- tryComp
      -- XXX checkme. we want to be absolutely certain that the top of the stack will
      -- be the just pushed handler frame.
      -- we have to nullify the top handler because the elseComp should not be
      -- executed in the context of the recently pushed handler. We can't simply
      -- pop the stack because we may have to execute a finally clause.
      nullifyTopHandler
      -- this is only executed if the tryComp does not raise an exception. Control
      -- would not reach here if an exception was raised.
      elseComp
   _ <- unwind isExceptionHandler
   pass

{- Python docs:
For an except clause with an expression, that expression is evaluated, and the clause matches the exception if the resulting object is “compatible” with the exception. An object is compatible with an exception if it is the class or a base class of the exception object or a tuple containing an item compatible with the exception.
-}

except :: Object -> Object -> Eval Object -> Eval Object -> Eval Object
except exceptionObj baseObj match noMatch = do
   BELCH("compatible check: " ++ show (exceptionObj, baseObj))
   isCompatible <- compatibleException exceptionObj baseObj
   if isCompatible
      then match
      else noMatch

-- XXX fixme, this is not correct, should also check if the exception is a subclass of the baseObj
compatibleException :: Object -> Object -> Eval Bool
compatibleException exceptionObj baseObj = do
   let typeOfException = typeOf exceptionObj
   objectEquality typeOfException baseObj

exceptDefault :: Eval Object -> Eval Object -> Eval Object
exceptDefault match _noMatch = match

{-
raise_stmt ::=  "raise" [expression ["from" expression]]
If no expressions are present, raise re-raises the last exception that was active in the current scope. If no exception is active in the current scope, a TypeError exception is raised indicating that this is an error (if running under IDLE, a queue.Empty exception is raised instead).

Otherwise, raise evaluates the first expression as the exception object. It must be either a subclass or an instance of BaseException. If it is a class, the exception instance will be obtained when needed by instantiating the class with no arguments.

The type of the exception is the exception instance’s class, the value is the instance itself.
-}

raise :: Object -> Eval Object
raise obj = do
   BELCH("Raising: " ++ show obj)
   IF_DEBUG(dumpStack)
   exceptionObj <- case obj of
      Type { object_constructor = cons } -> 
         callProcedure cons []
      other -> return other
   stack <- gets control_stack
   handleFrame exceptionObj stack
   where
   handleFrame :: Object -> ControlStack -> Eval Object
   handleFrame exceptionObj EmptyStack = do
     str <- showObject exceptionObj
     throw $ UncaughtException str
   handleFrame exceptionObj (ExceptionHandler { exception_handler = handler, exception_finally = finally }) = do
      -- BELCH("ExceptionHandler frame")
      case handler of
         -- this is a nullified handler. We (possibly) execute the finally clause 
         -- and keep unwinding.
         Nothing -> do
            -- it is important to pop the stack _before_ executing the finally clause,
            -- otherwise the finally clause would be executed in the wrong context.
            pop
            _ <- maybe pass id finally
            raise exceptionObj
         Just handlerAction -> do
            -- note we do not pop the stack here because we want the (possible) finally clause
            -- to remain on top of the stack. Instead we nullify the handler so that it is not
            -- executed again by a subsequent nested raise.
            nullifyTopHandler
            handlerAction exceptionObj
   -- if we walk past a GeneratorCall then we need to smash the continuation to always raise an
   -- exception
   handleFrame exceptionObj (GeneratorCall { generator_object = genObj }) = do
      writeIORef (object_continuation genObj) (raise stopIteration)
      pop >> raise exceptionObj
   handleFrame exceptionObj _other = do
      -- BELCH("other frame")
      pop >> raise exceptionObj
   

-- XXX fixme
-- This requires that we store the last raised exception somewhere
-- possibly in an activation record?
reRaise :: Eval Object
reRaise = error "reRaise not implemented"

-- XXX fixme
raiseFrom :: Object -> Object -> Eval Object
raiseFrom = error "raiseFrom not implemented"

yield :: Object -> Eval Object 
yield obj = do
   BELCH("Yielding " ++ show obj)
   -- IF_DEBUG(dumpStack)
   callCC $ \next -> do
      generatorYield <- unwindYieldContext (next none)
      generatorYield obj

-- the next method for generators
generatorNext :: [Object] -> Eval Object
generatorNext (obj:_) = do
   result <- callCC $ \next ->
      case obj of
         Generator {} -> do
            BELCH("Starting generator")
            stackContext <- readIORef $ object_stack_context obj
            push (stackContext . GeneratorCall next obj)
            BELCH("calling continuation")
            action <- readIORef $ object_continuation obj
            _ <- action
            BELCH("raising exception")
            raise stopIteration
         _other -> error "next applied to object which is not a generator"
   ret result
generatorNext [] = error "Generator applied to no arguments"

def :: Arity -> Object -> ([ObjectRef] -> Eval Object) -> Eval Object
def arity docString fun = do
   globalScope <- getGlobalScopeHashTable
   let procedureObj = function arity closure (Just globalScope)
   _ <- setattr procedureObj specialDocName docString
   return procedureObj
   where
   closure :: Procedure
   closure params = do
      argsRefs <- mapM newIORef params
      fun argsRefs

lambda :: Arity -> ([ObjectRef] -> Eval Object) -> Eval Object
lambda arity fun = do
   globalScope <- getGlobalScopeHashTable
   return $ function arity closure (Just globalScope)
   where
   closure :: Procedure
   closure params = do
      argsRefs <- mapM newIORef params
      fun argsRefs

returnGenerator :: Eval Object -> Eval Object
returnGenerator cont = do
   generatorObj <- generator cont
   ret generatorObj

printObject :: Object -> Eval ()
printObject obj = do
   str <- showObject obj
   -- stdout <- getStdout
   -- hPutStr stdout str
   withStdout $ \h -> hPutStr h str

showObject :: Object -> Eval String
-- XXX this should really choose the right quotes based on the content of the string.
showObject obj@(String {}) = return ("'" ++ object_string obj ++ "'")
showObject obj = object_string <$> callSpecialMethod obj specialStrName []

pyCallCC :: Object -> Eval Object
pyCallCC fun =
   callCC $ \ret -> do
      context <- getControlStack
      let cont = function 1
                          (\(obj:_) -> do
                             -- XXX should this run finalisers on the way out?
                             setControlStack context
                             ret obj)
                          Nothing
      -- XXX can this be a tail call?
      fun @@ [cont]

next :: Object -> Eval Object
next obj = callMethod obj specialNextName []

unpack :: Pat -> Object -> Eval Object
unpack (V var) obj = writeIORef var obj >> return none
unpack (G n pats) (Tuple { object_tuple = elements, object_length = size })
   | n == size = zipWithM unpack pats elements >> return none
   | otherwise = raise valueError
unpack (G n pats) (List { object_list_elements = elementsRef, object_list_num_elements = sizeRef }) = do
   size <- readIORef sizeRef
   if fromIntegral n == size
      then do
         elementsArray <- readIORef elementsRef
         objs <- liftIO $ getElems elementsArray
         _ <- zipWithM unpack pats objs
         return none
      else raise valueError
-- XXX this has different semantics than Python because it will allow pattern variables
-- to be assigned up-to the point an exception is raised. Python is all or nothing.
unpack (G _n pats) obj = do
   iterator <- getIterator obj
   unpackIterator pats iterator
{-
   iteratorTest <- isIterator obj
   if iteratorTest
      then do
         iterator <- callMethod obj specialIterName []
         unpackIterator pats iterator
      else
         raise valueError
-}
   where
   unpackIterator :: [Pat] -> Object -> Eval Object
   -- check that the iterator was exhausted, by looking for a stopIteration
   unpackIterator [] _obj =
      tryElse (next obj) handler (raise valueError)
      where
      handler e = except e stopIteration pass (raise e)
   unpackIterator (pat:pats) obj = do
      _ <- try assignNext handler
      unpackIterator pats obj
      where
      assignNext :: Eval Object
      assignNext = unpack pat =<< next obj
      handler :: Object -> Eval Object
      handler e = except e stopIteration (raise valueError) (raise e)

getIterator :: Object -> Eval Object
getIterator obj = do
   iteratorTest <- isIterator obj
   if iteratorTest
      then callMethod obj specialIterName []
      else raise valueError

mapIterator :: (Object -> Eval ()) -> Object -> Eval ()
mapIterator f obj = do
   iterator <- getIterator obj
   _ <- mapWorker iterator
   return ()
   where
   mapWorker :: Object -> Eval Object
   mapWorker iterObj = do
      tryElse mapNext handler $ mapWorker iterObj
      where
      handler e = except e stopIteration pass (raise e)
      mapNext = do
         f =<< next iterObj
         pass

importModuleRef :: FilePath -> Eval ObjectRef
importModuleRef path = newIORef =<< importModule path

importModule :: FilePath -> Eval Object
importModule path = do
   maybeImported <- lookupModuleCache path
   case maybeImported of
      Just obj -> return obj
      Nothing -> do
         obj <- compileModuleAndLoadInit path
         updateModuleCache path obj
         return obj

compileModuleAndLoadInit :: FilePath -> Eval Object
compileModuleAndLoadInit path = do
{-
   maybePath <- findModulePath name
   case maybePath of
      Nothing -> raise ("could not find module")
      Just path -> do
         compiled <- isCompiled path
         if compiled
            then liftIO $ load path "init"
            else do
-}
               objFile <- liftIO $ compilePythonToObjectFile path -- may raise exception
               loadStatus <- liftIO $ load_ objFile [] "init" -- should catch haskell exceptions here
               case loadStatus of
                  LoadSuccess _module init -> init
                  LoadFailure errs -> error ("load failed: " ++ show errs)

{-
mkModule :: Eval Object
mkModule = do
   dict <- mkAttributes =<< getGlobalScopeHashTable
   identity <- newIdentity
   return $
      Module { object_identity = identity
             , object_dict = dict }
-}

pushGlobalScope :: Maybe HashTable -> Eval ()
-- we don't have a readl hashtable to push (ie for prims) so we duplicate the top
-- of the stack, to make sure we have something to pop later
pushGlobalScope Nothing = do
   scope <- gets state_global_scope
   let hashTable = global_scope_bindings scope
   let newScope = NestedGlobalScope hashTable scope
   modify $ \state -> state { state_global_scope = newScope }
pushGlobalScope (Just hashTable) = do
   scope <- gets state_global_scope
   let newScope = NestedGlobalScope hashTable scope
   modify $ \state -> state { state_global_scope = newScope }

popGlobalScope :: Eval ()
popGlobalScope = do
   scope <- gets state_global_scope
   case scope of
      NestedGlobalScope {} ->
         modify $ \state -> state { state_global_scope = global_scope_tail scope }
      -- XXX this should be an exception
      TopGlobalScope {} -> error "tried to pop the top global scope"

modifyGlobalScope :: Maybe HashTable -> Eval ()
-- we don't have a real hashtable, so leave the global scope alone
-- eg we are doing a tail call on a primitive.
modifyGlobalScope Nothing = return ()
modifyGlobalScope (Just hashTable) = do
   oldHashTable <- getGlobalScopeHashTable
   newTable <- readIORef hashTable
   writeIORef oldHashTable newTable
