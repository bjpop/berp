-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}
#include "BerpDebug.h"

module Berp.Base.ControlStack 
   ( isEmpty, isProcedureCall, isExceptionHandler, isWhileLoop, isGeneratorCall
   , unwind, unwindPastWhileLoop, unwindUpToWhileLoop, push, pop, nullifyTopHandler 
   , unwindYieldContext, dumpStack
   )
   where

import Control.Monad.State
import Data.Maybe (maybe)
import Data.IORef (writeIORef)
import Berp.Base.SemanticTypes (ControlStack (..), Eval, EvalState (..), Object (..))

isEmpty :: ControlStack -> Bool
isEmpty EmptyStack = True
isEmtpy _ = False

isProcedureCall :: ControlStack -> Bool
isProcedureCall (ProcedureCall {}) = True
isProcedureCall _ = False

isExceptionHandler :: ControlStack -> Bool
isExceptionHandler (ExceptionHandler {}) = True
isExceptionHandler _ = False

isWhileLoop :: ControlStack -> Bool
isWhileLoop (WhileLoop {}) = True
isWhileLoop _ = False

isGeneratorCall :: ControlStack -> Bool
isGeneratorCall (GeneratorCall {}) = True
isGeneratorCall _ = False

{- Unwind the control stack and execute any "finally" exception handlers
   that we pass along the way. Returns the stack with the most recently popped 
   element remaining.
-}
unwind :: (ControlStack -> Bool) -> Eval ControlStack 
unwind pred = do
   stack <- gets control_stack
   unwindFrame stack
   where
   unwindFrame :: ControlStack -> Eval ControlStack 
   -- XXX should be an exception
   unwindFrame EmptyStack = error $ "unwindFrame: empty control stack" 
   unwindFrame stack@(ExceptionHandler { exception_finally = maybeFinally }) = do
      pop
      maybe (return ()) id maybeFinally
      if pred stack
         then return stack 
         else unwind pred 
   unwindFrame stack
      | pred stack = pop >> return stack 
      | otherwise = pop >> unwind pred 

unwindYieldContext :: Eval () -> Eval (Object -> Eval ()) 
unwindYieldContext continuation = do
   stack <- gets control_stack
   let (generatorYield, generatorObj, newStack, context) = unwindYieldWorker stack
   liftIO $ writeIORef (object_continuation generatorObj) continuation 
   liftIO $ writeIORef (object_stack_context generatorObj) context
   setControlStack newStack
   return generatorYield 
   where
   unwindYieldWorker :: ControlStack -> (Object -> Eval (), Object, ControlStack, ControlStack -> ControlStack) 
   -- XXX this should be an exception
   unwindYieldWorker EmptyStack = error "unwindYieldWorker: empty control stack"
   unwindYieldWorker (ProcedureCall {}) = error "unwindYieldWorker: procedure call"
   unwindYieldWorker (ExceptionHandler handler finally tail) =
      (yield, obj, stack, ExceptionHandler handler finally . context)
      where
      (yield, obj, stack, context) = unwindYieldWorker tail
   unwindYieldWorker (WhileLoop start end tail) =
      (yield, obj, stack, WhileLoop start end . context)
      where
      (yield, obj, stack, context) = unwindYieldWorker tail
   unwindYieldWorker (GeneratorCall yield obj tail) = (yield, obj, tail, id)

unwindPastWhileLoop :: Eval ControlStack
unwindPastWhileLoop = do
   stack <- unwindUpToWhileLoop
   pop
   return stack

unwindUpToWhileLoop :: Eval ControlStack
unwindUpToWhileLoop = do
   stack <- gets control_stack
   unwindFrame stack
   where
   unwindFrame :: ControlStack -> Eval ControlStack 
   -- XXX should be an exception, should mention continue/break called outside of loop
   unwindFrame EmptyStack = error $ "unwindUpToWhileLoop: empty control stack"
   unwindFrame (ExceptionHandler { exception_finally = maybeFinally }) = do
      pop
      maybe (return ()) id maybeFinally
      unwindUpToWhileLoop
   unwindFrame stack@(WhileLoop {}) = return stack
   -- XXX should be an exception which mentions continue/break called outside of loop
   unwindFrame (ProcedureCall {}) = error $ "unwindUpToWhileLoop: procedure call"
   unwindFrame (GeneratorCall {}) = error $ "unwindUpToWhileLoop: generator call"

pop :: Eval ()
pop = do
   stack <- gets control_stack
   case stack of
      -- should be an exception
      EmptyStack -> error "pop: empty stack" 
      other -> setControlStack $ control_stack_tail stack

push :: (ControlStack -> ControlStack) -> Eval ()
push frame = do
   stack <- gets control_stack
   setControlStack (frame stack) 

setControlStack :: ControlStack -> Eval ()
setControlStack stack = modify $ \state -> state { control_stack = stack }

-- assumes top of stack is an exception handler
nullifyTopHandler :: Eval ()
nullifyTopHandler = do
   IF_DEBUG(dumpStack)
   stack <- gets control_stack
   case stack of
     ExceptionHandler {} -> 
        setControlStack $ stack { exception_handler = Nothing } 
     other -> error $ "nullifyTopHandler: top of stack is not an exception handler: " ++ show stack

dumpStack :: Eval ()
dumpStack = do
   liftIO $ putStrLn "--- Bottom of stack ---"
   stack <- gets control_stack
   mapStackM printer stack
   liftIO $ putStrLn "--- Top of stack ---"
   where
   printer :: ControlStack -> Eval ()
   printer (ProcedureCall {}) = liftIO $ putStrLn "ProcedureCall" 
   printer (ExceptionHandler {}) = liftIO $ putStrLn "ExceptionHandler" 
   printer (WhileLoop {}) = liftIO $ putStrLn "WhileLoop" 
   printer (GeneratorCall {}) = liftIO $ putStrLn "GeneratorCall" 

mapStackM :: Monad m => (ControlStack -> m ()) -> ControlStack -> m ()
mapStackM f EmptyStack = return ()
mapStackM f stack = f stack >> mapStackM f (control_stack_tail stack)
