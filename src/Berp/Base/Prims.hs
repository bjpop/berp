{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PatternGuards, TemplateHaskell #-}

-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}

-- uncomment one of the two above lines to turn debugging on/off for this module
#include "BerpDebug.h"

module Berp.Base.Prims 
   ( (=:), stmt, ifThenElse, ret, pass, break
   , continue, while, whileElse, for, forElse, ifThen, (@@)
   , read, var, binOp, setattr, callMethod, subs
   , try, tryElse, tryFinally, tryElseFinally, except, exceptDefault
   , raise, reRaise, raiseFrom, primitive, generator, yield, generatorNext
   , def, lambda, mkGenerator) where

import Prelude hiding (break, read)
import Control.Monad.State (gets)
import Control.Monad.Cont (callCC)
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.IORef  
import Data.Maybe (maybe)
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Object (..), ObjectRef, Procedure, Eval, EvalState(..), ControlStack(..), Arity)
import Berp.Base.Truth (truth)
import {-# SOURCE #-} Berp.Base.Object (typeOf, dictOf, lookupAttribute, objectEquality)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.Mangle (deMangle)
import Berp.Base.ControlStack
import Berp.Base.StdNames (docName) 
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (function)
import {-# SOURCE #-} Berp.Base.HashTable as Hash (stringInsert, insert)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (true, false)
import {-# SOURCE #-} Berp.Base.StdTypes.Generator (generator)
import {-# SOURCE #-} Berp.Base.Builtins.Exception (stopIteration, typeError)

primitive :: Arity -> Procedure -> Object
primitive arity proc =  
   function arity $ \args -> do
      result <- proc args
      ret result
      -- we need the result to be "Eval Object", 
      -- even though execution never gets here because of the preceeding return
      return none

infix 1 =:  -- assignment
infixl 8 @@ -- procedure application

var :: Hashed Ident -> Eval ObjectRef
var (_, s) = liftIO $ newIORef (error $ "undefined variable: " ++ deMangle s)

{-
globalVar :: Hashed Ident -> Eval ObjectRef
globalVar ident@(_, s) = do
   ref <- var ident
   updateGlobalEnv s ref
   return ref
-}

read :: ObjectRef -> Eval Object
read = liftIO . readIORef 

ret :: Object -> Eval ()
ret obj = do
   stack <- unwind isProcedureCall
   procedure_return stack obj

pass :: Eval ()
pass = return ()

break :: Eval ()
break = do
   stack <- unwindPastWhileLoop 
   loop_end stack

continue :: Eval ()
continue = do 
   stack <- unwindUpToWhileLoop 
   loop_start stack

(=:) :: ObjectRef -> Object -> Eval ()
ident =: obj = liftIO $ writeIORef ident obj 

-- XXX we could have specialised versions for certain small arities and thus
-- dispense with the list of objects
(@@) :: Object -> [Object] -> Eval Object 
obj @@ args = do
    case obj of 
        Function { object_procedure = proc, object_arity = arity }
           | arity == length args -> 
                callCC $ \ret -> do 
                   push $ ProcedureCall ret
                   proc args 
           -- XXX should be raise of arity, typeError exception
           | otherwise -> raise typeError >> return none
        Type { object_constructor = proc } -> proc args
        -- XXX should try to find "__call__" attribute on object
        other -> raise typeError >> return none 

ifThenElse :: Eval Object -> Eval () -> Eval () -> Eval () 
ifThenElse condComp trueComp falseComp = do
    cond <- condComp
    if truth cond then trueComp else falseComp

ifThen :: Eval Object -> Eval () -> Eval ()
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

for :: ObjectRef -> Object -> Eval () -> Eval ()
for var exp body = forElse var exp body (return ()) 

forElse :: ObjectRef -> Object -> Eval () -> Eval () -> Eval ()
forElse var expObj suite1 suite2 = do
   iterObj <- callMethod expObj $(hashedStr "__iter__") [] -- this could be specialised
   cond <- liftIO $ newIORef true
   let tryBlock = do nextObj <- callMethod iterObj $(hashedStr "__next__") [] -- this could be specialised
                     liftIO $ writeIORef var nextObj
                     suite1
   let handler e = except e stopIteration (liftIO $ writeIORef cond false) (raise e) 
   let whileBlock = try tryBlock handler
   whileElse (liftIO $ readIORef cond) whileBlock suite2

while :: Eval Object -> Eval () -> Eval () 
while cond loopBlock = whileElse cond loopBlock pass 

whileElse :: Eval Object -> Eval () -> Eval () -> Eval () 
whileElse cond loopBlock elseBlock = do
   callCC $ \end -> do 
      let afterLoop = end () 
          loop = do condVal <- cond
                    if truth condVal
                       then do
                          loopBlock 
                          loop
                       -- this does the unwind before the else block,
                       -- otherwise a call to break or continue in the else block
                       -- would have undesired results
                       else do
                          unwindPastWhileLoop
                          elseBlock 
                          afterLoop 
      push $ WhileLoop loop afterLoop
      loop

stmt :: Eval Object -> Eval ()
stmt comp = comp >> pass 

-- XXX could this be turned into a type class?
binOp :: Object -> Object -> (Object -> t) -> (t -> t -> r) -> (r -> Eval Object) -> Eval Object
binOp left right project fun build 
   = build (project left `fun` project right)

{-
global :: Hashed Ident -> Eval Object
global ident = do
   ref <- globalRef ident 
   liftIO $ readIORef ref
-}

-- globalRef :: Ident -> Eval ObjectRef
{-
globalRef :: Hashed Ident -> Eval ObjectRef
globalRef (_, ident) = do
   globals <- gets global_env 
   maybeRef <- lookupVarEnv ident globals 
   case maybeRef of
      Nothing -> fail $ "undefined variable: " ++ deMangle ident
      Just ref -> return ref 
-}

-- XXX this should also work on Type
-- XXX need to support __setattr__ and descriptors
-- setattr :: Object -> Ident -> Object -> Eval ()
setattr :: Object -> Hashed String -> Object -> Eval ()
setattr target attribute value 
   | Just dict <- dictOf target = do
        let hashTable = object_hashTable dict
        Hash.stringInsert attribute value $ hashTable
   | otherwise = error $ "setattr on object unimplemented: " ++ show (target, attribute)
{-
setattr target@(Object {}) attribute value = do
   let hashTable = object_hashTable $ object_dict target
   Hash.stringInsert attribute value $ hashTable
setattr other attribute value = error $ "setattr on object unimplemented: " ++ show other 
-}

callMethod :: Object -> Hashed String -> [Object] -> Eval Object
callMethod object ident args = do
   proc <- liftIO $ lookupAttribute object ident
   proc @@ args

subs :: Object -> Object -> Eval Object
subs obj subscript = callMethod obj $(hashedStr "__getitem__") [subscript]

try :: Eval () -> (Object -> Eval ()) -> Eval ()
try tryComp handler = tryWorker tryComp handler pass Nothing 

tryElse :: Eval () -> (Object -> Eval ()) -> Eval () -> Eval ()
tryElse tryComp handler elseComp = 
   tryWorker tryComp handler elseComp Nothing 

tryFinally :: Eval () -> (Object -> Eval ()) -> Eval () -> Eval ()
tryFinally tryComp handler finallyComp 
   = tryWorker tryComp handler pass (Just finallyComp) 

tryElseFinally :: Eval () -> (Object -> Eval ()) -> Eval () -> Eval () -> Eval ()
tryElseFinally tryComp handler elseComp finallyComp 
   = tryWorker tryComp handler elseComp (Just finallyComp) 

tryWorker :: Eval () -> (Object -> Eval ()) -> Eval () -> Maybe (Eval ()) -> Eval ()
tryWorker tryComp handler elseComp maybeFinallyComp = do
   callCC $ \afterTry -> do
      push (ExceptionHandler 
              (Just $ \obj -> do
                   handler obj 
                   afterTry ()) 
              maybeFinallyComp)
      -- BELCH_EVAL("Before tryComp")
      tryComp
      -- XXX checkme. we want to be absolutely certain that the top of the stack will
      -- be the just pushed handler frame.
      -- we have to nullify the top handler because the elseComp should not be
      -- executed in the context of the recently pushed handler. We can't simply
      -- pop the stack because we may have to execute a finally clause.
      -- BELCH_EVAL("after tryComp")
      nullifyTopHandler
      -- this is only executed if the tryComp does not raise an exception. Control
      -- would not reach here if an exception was raised.
      elseComp
   unwind isExceptionHandler 
   pass 

{- Python docs:
For an except clause with an expression, that expression is evaluated, and the clause matches the exception if the resulting object is “compatible” with the exception. An object is compatible with an exception if it is the class or a base class of the exception object or a tuple containing an item compatible with the exception.
-}

except :: Object -> Object -> Eval () -> Eval () -> Eval ()
except exceptionObj baseObj match noMatch = do
   BELCH_EVAL("compatible check: " ++ show (exceptionObj, baseObj))
   isCompatible <- compatibleException exceptionObj baseObj
   if isCompatible
      then match
      else noMatch
   where
   -- XXX fixme, this is not correct
   compatibleException :: Object -> Object -> Eval Bool
   compatibleException exceptionObj baseObj = do
      let typeOfException = typeOf exceptionObj
      objectEquality typeOfException baseObj 

exceptDefault :: Eval () -> Eval () -> Eval ()
exceptDefault match _noMatch = match

{-
raise_stmt ::=  "raise" [expression ["from" expression]]
If no expressions are present, raise re-raises the last exception that was active in the current scope. If no exception is active in the current scope, a TypeError exception is raised indicating that this is an error (if running under IDLE, a queue.Empty exception is raised instead).

Otherwise, raise evaluates the first expression as the exception object. It must be either a subclass or an instance of BaseException. If it is a class, the exception instance will be obtained when needed by instantiating the class with no arguments.

The type of the exception is the exception instance’s class, the value is the instance itself.
-}

raise :: Object -> Eval ()
raise obj = do
   BELCH_EVAL("Raising: " ++ show obj)
   IF_DEBUG(dumpStack)
   exceptionObj <- case obj of
      Type { object_constructor = cons } -> cons []
      other -> return other
   stack <- gets control_stack
   handleFrame exceptionObj stack
   where
   handleFrame :: Object -> ControlStack -> Eval ()
   handleFrame exceptionObj EmptyStack = error ("uncaught exception: " ++ show exceptionObj)
   handleFrame exceptionObj (ExceptionHandler { exception_handler = handler, exception_finally = finally }) = do
      -- BELCH_EVAL("ExceptionHandler frame")
      case handler of
         -- this is a nullified handler. We (possibly) execute the finally clause 
         -- and keep unwinding.
         Nothing -> do
            -- it is important to pop the stack _before_ executing the finally clause,
            -- otherwise the finally clause would be executed in the wrong context.
            pop
            maybe pass id finally
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
      liftIO $ writeIORef (object_continuation genObj) (raise stopIteration)
      pop >> raise exceptionObj
   handleFrame exceptionObj other = do
      -- BELCH_EVAL("other frame")
      pop >> raise exceptionObj
   

-- XXX fixme
-- This requires that we store the last raised exception somewhere
-- possibly in an activation record?
reRaise :: Eval ()
reRaise = error "reRaise not implemented"

-- XXX fixme
raiseFrom :: Object -> Object -> Eval ()
raiseFrom = error "raiseFrom not implemented"

yield :: Object -> Eval Object 
yield obj = do
   BELCH_EVAL("Yielding " ++ show obj)
   -- IF_DEBUG(dumpStack)
   callCC $ \next -> do
      generatorYield <- unwindYieldContext (next ())
      generatorYield obj
   return none 

-- the next method for generators
generatorNext :: [Object] -> Eval Object
generatorNext (obj:_) = do
   result <- callCC $ \next ->
      case obj of
         Generator {} -> do
            -- BELCH_EVAL("Starting generator")
            stackContext <- liftIO $ readIORef $ object_stack_context obj
            push (stackContext . GeneratorCall next obj)
            -- BELCH_EVAL("calling continuation")
            action <- liftIO $ readIORef $ object_continuation obj
            action
            -- BELCH_EVAL("raising exception")
            raise stopIteration 
            return none
   ret result
   return none

def :: ObjectRef -> Arity -> Object -> ([ObjectRef] -> Eval ()) -> Eval () 
def ident arity docString fun = do
   let procedureObj = function arity closure
   setattr procedureObj docName docString
   liftIO $ writeIORef ident procedureObj
   where
   closure :: Procedure 
   closure params = do
      argsRefs <- liftIO $ mapM newIORef params 
      fun argsRefs 
      -- may not get here, but if you do, return None
      -- XXX revisit when considering tail call optimisation
      Prelude.return None 

lambda :: Arity -> ([ObjectRef] -> Eval Object) -> Eval Object
lambda arity fun = 
   return $ function arity closure 
   where
   closure :: Procedure 
   closure params = do
      argsRefs <- liftIO $ mapM newIORef params 
      fun argsRefs

mkGenerator :: Eval () -> Eval ()
mkGenerator cont = do
   generatorObj <- generator cont
   ret generatorObj
