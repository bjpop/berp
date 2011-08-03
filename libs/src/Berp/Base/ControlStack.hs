-- {-# OPTIONS_GHC -cpp -DDEBUG #-}
{-# OPTIONS_GHC -cpp #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.ControlStack
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Operations on the control stack.
--
-----------------------------------------------------------------------------

#include "BerpDebug.h"

module Berp.Base.ControlStack 
   ( isEmpty, isProcedureCall, isExceptionHandler, isWhileLoop, isGeneratorCall
   , unwind, unwindPastWhileLoop, unwindUpToWhileLoop, push, pop, nullifyTopHandler 
   , unwindYieldContext, dumpStack, getControlStack, setControlStack
   )
   where

import Control.Monad.State
import Berp.Base.SemanticTypes (ControlStack (..), Eval, EvalState (..), Object (..))
import Berp.Base.LiftedIO as LIO (writeIORef, putStrLn)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)

isEmpty :: ControlStack -> Bool
isEmpty EmptyStack = True
isEmpty _ = False

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
   stack <- gets state_control_stack
   unwindFrame stack
   where
   unwindFrame :: ControlStack -> Eval ControlStack 
   -- XXX should be an exception
   unwindFrame EmptyStack = error $ "unwindFrame: empty control stack" 
   unwindFrame stack@(ExceptionHandler { exception_finally = maybeFinally }) = do
      pop
      _ <- maybe (return none) id maybeFinally
      if pred stack
         then return stack 
         else unwind pred 
   unwindFrame stack
      | pred stack = pop >> return stack 
      | otherwise = pop >> unwind pred 

unwindYieldContext :: Eval Object -> Eval (Object -> Eval Object) 
unwindYieldContext continuation = do
   stack <- gets state_control_stack
   let (generatorYield, generatorObj, newStack, context) = unwindYieldWorker stack
   LIO.writeIORef (object_continuation generatorObj) continuation 
   LIO.writeIORef (object_stack_context generatorObj) context
   setControlStack newStack
   return generatorYield 
   where
   unwindYieldWorker :: ControlStack -> (Object -> Eval Object, Object, ControlStack, ControlStack -> ControlStack) 
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
   stack <- gets state_control_stack
   unwindFrame stack
   where
   unwindFrame :: ControlStack -> Eval ControlStack 
   -- XXX should be an exception, should mention continue/break called outside of loop
   unwindFrame EmptyStack = error $ "unwindUpToWhileLoop: empty control stack"
   unwindFrame (ExceptionHandler { exception_finally = maybeFinally }) = do
      pop
      _ <- maybe (return none) id maybeFinally
      unwindUpToWhileLoop
   unwindFrame stack@(WhileLoop {}) = return stack
   -- XXX should be an exception which mentions continue/break called outside of loop
   unwindFrame (ProcedureCall {}) = error $ "unwindUpToWhileLoop: procedure call"
   unwindFrame (GeneratorCall {}) = error $ "unwindUpToWhileLoop: generator call"

pop :: Eval ()
pop = do
   stack <- gets state_control_stack
   case stack of
      -- should be an exception
      EmptyStack -> error "pop: empty stack" 
      _other -> setControlStack $ control_stack_tail stack

push :: (ControlStack -> ControlStack) -> Eval ()
push frame = do
   stack <- gets state_control_stack
   setControlStack (frame stack) 

setControlStack :: ControlStack -> Eval ()
setControlStack stack = modify $ \state -> state { state_control_stack = stack }

getControlStack :: Eval ControlStack
getControlStack = gets state_control_stack

-- assumes top of stack is an exception handler
nullifyTopHandler :: Eval ()
nullifyTopHandler = do
   IF_DEBUG(dumpStack)
   stack <- gets state_control_stack
   case stack of
     ExceptionHandler {} -> 
        setControlStack $ stack { exception_handler = Nothing } 
     _other -> error $ "nullifyTopHandler: top of stack is not an exception handler: " ++ show stack

dumpStack :: Eval ()
dumpStack = do
   LIO.putStrLn "--- Bottom of stack ---"
   stack <- gets state_control_stack
   mapStackM printer stack
   LIO.putStrLn "--- Top of stack ---"
   where
   printer :: ControlStack -> Eval ()
   printer (ProcedureCall {}) = LIO.putStrLn "ProcedureCall" 
   printer (ExceptionHandler {}) = LIO.putStrLn "ExceptionHandler" 
   printer (WhileLoop {}) = LIO.putStrLn "WhileLoop" 
   printer (GeneratorCall {}) = LIO.putStrLn "GeneratorCall" 
   printer (EmptyStack {}) = LIO.putStrLn "EmptyStack" 

mapStackM :: Monad m => (ControlStack -> m ()) -> ControlStack -> m ()
mapStackM _f EmptyStack = return ()
mapStackM f stack = f stack >> mapStackM f (control_stack_tail stack)
