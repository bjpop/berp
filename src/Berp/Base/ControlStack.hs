module Berp.Base.ControlStack where

import Berp.Base.SemanticTypes (ControlStack (..), Eval, EvalState (..))
import Control.Monad.State
import Data.Maybe (maybe)

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
   stack <- gets control_stack
   case stack of
     ExceptionHandler {} -> 
        setControlStack $ stack { exception_handler = Nothing } 
     other -> error $ "nullifyTopHandler: top of stack is not an exception handler: " ++ show stack
