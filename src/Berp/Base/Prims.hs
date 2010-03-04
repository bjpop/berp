{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PatternGuards, TemplateHaskell #-}

module Berp.Base.Prims 
   ( pure, (=:), stmt, ifThenElse, return, pass, break
   , continue, while, whileElse, ifThen, (@@), global, globalRef
   , read, var, globalVar, binOp, setattr, callMethod, subs
   , try, tryElse, tryFinally, tryElseFinally, except, exceptDefault
   , raise, reRaise, raiseFrom, primitive) where

import Prelude hiding (return, break, read)
import Control.Applicative (pure)
import Control.Monad.State (gets)
import Control.Monad.Cont (callCC)
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import qualified Prelude (return)
import Data.IORef  -- XXX Can we eliminate the import of IORef?
import Data.Maybe (maybe)
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Object (..), ObjectRef, Procedure, Eval, EvalState(..), ControlStack(..), Arity)
import Berp.Base.Env (lookupVarEnv, updateGlobalEnv, updateVarEnv)
import Berp.Base.Truth (truth)
import Berp.Base.Object (lookupAttribute)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.Mangle (deMangle)
import Berp.Base.ControlStack
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (function)
import {-# SOURCE #-} Berp.Base.HashTable as Hash (insert)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (true, false)

primitive :: Arity -> Procedure -> Object
primitive arity proc =  
   function arity $ \args -> do
      result <- proc args
      return result
      -- we need the result to be "Eval Object", 
      -- even though execution never gets here because of the preceeding return
      Prelude.return none

infix 1 =:  -- assignment
infixl 8 @@ -- procedure application

var :: Hashed Ident -> Eval ObjectRef
var (_, s) = liftIO $ newIORef (error $ "undefined variable: " ++ deMangle s)

globalVar :: Hashed Ident -> Eval ObjectRef
globalVar ident@(_, s) = do
   ref <- var ident
   updateGlobalEnv s ref
   Prelude.return ref

read :: ObjectRef -> Eval Object
read = liftIO . readIORef 

return :: Object -> Eval ()
return obj = do
   stack <- unwind isProcedureCall
   case stack of
      ProcedureCall { procedure_return = ret } -> ret obj

pass :: Eval ()
pass = Prelude.return ()

break :: Eval ()
break = do
   -- top of control stack may not be a while frame, so we unwind to find it (and go past it).
   stack <- unwindPastWhileLoop 
   case stack of
      WhileLoop { loop_end = end } -> end 

continue :: Eval ()
continue = do 
   stack <- unwindUpToWhileLoop 
   case stack of
      WhileLoop { loop_start = start } -> start 

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
           | otherwise -> raise none >> Prelude.return none
        Type { object_constructor = proc } -> proc args
        -- XXX should try to find "__call__" attribute on object
        other -> fail $ "attempt to call a non procedure/non class: " ++ show obj

ifThenElse :: Eval Object -> Eval () -> Eval () -> Eval () 
ifThenElse condComp trueComp falseComp = do
    cond <- condComp
    if truth cond then trueComp else falseComp

ifThen :: Eval Object -> Eval () -> Eval ()
ifThen condComp trueComp = do
   cond <- condComp
   if truth cond then trueComp else Prelude.return ()

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

for :: ObjectRef -> Object -> Eval () -> Eval () -> Eval ()
for var expObj suite1 suite2 = do
   iterObj <- callMethod expObj $(hashedStr "__iter__") [] -- this could be specialised
   cond <- liftIO $ newIORef true
   let tryBlock = do nextObj <- callMethod iterObj $(hashedStr "__next__") [] -- this could be specialised
                     liftIO $ writeIORef var nextObj
                     suite1
   -- XXX fixme
   let stopIteration = none
   let handler e = except e stopIteration (liftIO $ writeIORef cond false) (raise e) 
   let whileBlock = try tryBlock handler
   whileElse (liftIO $ readIORef cond) whileBlock suite2

while :: Eval Object -> Eval () -> Eval () 
while cond loopBlock = whileElse cond loopBlock (Prelude.return ())

whileElse :: Eval Object -> Eval () -> Eval () -> Eval () 
whileElse cond loopBlock elseBlock = do
   callCC $ \end -> do 
      let afterLoop = end undefined
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
stmt comp = comp >> Prelude.return ()

-- XXX could this be turned into a type class?
binOp :: Object -> Object -> (Object -> t) -> (t -> t -> r) -> (r -> Eval Object) -> Eval Object
binOp left right project fun build 
   = build (project left `fun` project right)

global :: Hashed Ident -> Eval Object
global ident = do
   ref <- globalRef ident 
   liftIO $ readIORef ref

-- globalRef :: Ident -> Eval ObjectRef
globalRef :: Hashed Ident -> Eval ObjectRef
globalRef (_, ident) = do
   globals <- gets global_env 
   maybeRef <- lookupVarEnv ident globals 
   case maybeRef of
      Nothing -> fail $ "undefined variable: " ++ deMangle ident
      Just ref -> Prelude.return ref 

-- XXX this should also work on Type
-- XXX need to support __setattr__ and descriptors
setattr :: Object -> Ident -> Object -> Eval ()
setattr target@(Object {}) attribute value = do
   let hashTable = object_hashTable $ object_dict target
   Hash.insert (string attribute) value $ hashTable
setattr other attribute value = error $ "setattr on object unimplemented: " ++ show other 

callMethod :: Object -> Hashed String -> [Object] -> Eval Object
callMethod object ident args = do
   proc <- liftIO $ lookupAttribute object ident
   proc @@ args

subs :: Object -> Object -> Eval Object
subs obj subscript = callMethod obj $(hashedStr "__getitem__") [subscript]

try :: Eval () -> (Object -> Eval ()) -> Eval ()
try tryComp handler = 
   tryWorker tryComp handler (Prelude.return ()) Nothing 

tryElse :: Eval () -> (Object -> Eval ()) -> Eval () -> Eval ()
tryElse tryComp handler elseComp = 
   tryWorker tryComp handler elseComp Nothing 

tryFinally :: Eval () -> (Object -> Eval ()) -> Eval () -> Eval ()
tryFinally tryComp handler finallyComp 
   = tryWorker tryComp handler (Prelude.return ()) (Just finallyComp) 

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
      tryComp
      -- XXX checkme. we want to be absolutely certain that the top of the stack will
      -- be the just pushed handler frame.
      -- we have to nullify the top handler because the elseComp should not be
      -- executed in the context of the recently pushed handler. We can't simply
      -- pop the stack because we may have to execute a finally clause.
      nullifyTopHandler
      -- this is only executed if the tryComp does not raise an exception. Control
      -- would not reach here if an exception was raised.
      elseComp
   unwind isExceptionHandler 
   Prelude.return ()

except :: Object -> Object -> Eval () -> Eval () -> Eval ()
except e baseObj match noMatch = do
   isCompatible <- compatible e baseObj
   if isCompatible
      then match
      else noMatch

exceptDefault :: Eval () -> Eval () -> Eval ()
exceptDefault match _noMatch = match

-- XXX fixme
compatible :: Object -> Object -> Eval Bool
compatible e base = Prelude.return True

raise :: Object -> Eval ()
raise obj = do
   stack <- gets control_stack
   handleFrame stack
   where
   handleFrame :: ControlStack -> Eval ()
   handleFrame EmptyStack = error ("uncaught exception: " ++ show obj)
   handleFrame (ExceptionHandler { exception_handler = handler, exception_finally = finally }) =
      case handler of
         -- this is a nullified handler. We (possibly) execute the finally clause 
         -- and keep unwinding.
         Nothing -> do
            -- it is important to pop the stack _before_ executing the finally clause,
            -- otherwise the finally clause would be executed in the wrong context.
            pop
            maybe (Prelude.return ()) id finally
            raise obj 
         Just handlerAction -> do
            -- note we do not pop the stack here because we want the (possible) finally clause
            -- to remain on top of the stack. Instead we nullify the handler so that it is not
            -- executed again by a subsequent nested raise.
            nullifyTopHandler
            handlerAction obj
   handleFrame other = pop >> raise obj

-- XXX fixme
-- This requires that we store the last raised exception somewhere
-- possibly in an activation record?
reRaise :: Eval ()
reRaise = error "reRaise not implemented"

-- XXX fixme
raiseFrom :: Object -> Object -> Eval ()
raiseFrom = error "raiseFrom not implemented"
