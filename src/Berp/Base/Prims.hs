{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PatternGuards, TemplateHaskell #-}

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
   , def, lambda, mkGenerator, printObject, topVar, Applicative.pure
   , pureObject, showObject, returningProcedure ) where

import System.Exit (exitWith)
import Prelude hiding (break, read, putStr)
import Control.Monad.State (gets)
import Control.Monad.Cont (callCC)
import Control.Monad (join)
import Berp.Base.LiftedIO as LIO (readIORef, writeIORef, newIORef, putStr, putStrLn {- needed for BELCH -})
import qualified Control.Applicative as Applicative (pure)
import Control.Applicative ((<$>))
import Data.Maybe (maybe)
import Berp.Base.ExitCodes (uncaughtExceptionError)
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (Object (..), ObjectRef, Procedure, Eval, EvalState(..), ControlStack(..), Arity)
import Berp.Base.Truth (truth)
import {-# SOURCE #-} Berp.Base.Object 
   ( typeOf, dictOf, lookupAttribute, lookupSpecialAttribute
   , lookupAttributeMaybe, objectEquality)
import Berp.Base.Hash (Hashed, hashedStr)
import Berp.Base.Mangle (deMangle)
import Berp.Base.ControlStack
import Berp.Base.StdNames (docName, strName) 
import Berp.Base.Exception (RuntimeError (..), throw)
import {-# SOURCE #-} Berp.Base.StdTypes.String (string)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (function)
import {-# SOURCE #-} Berp.Base.HashTable as Hash (stringInsert, insert)
import {-# SOURCE #-} Berp.Base.StdTypes.None (none)
import {-# SOURCE #-} Berp.Base.StdTypes.Bool (true, false)
import {-# SOURCE #-} Berp.Base.StdTypes.Generator (generator)
import {-# SOURCE #-} Berp.Base.Builtins.Exceptions (stopIteration, typeError)

-- specialised to monomorphic type for the benefit of the interpreter.
-- otherwise we'd need to add a type annotation in the generated code.
pureObject :: Object -> Eval Object
pureObject = Applicative.pure

primitive :: Arity -> Procedure -> Object
primitive arity = function arity . returningProcedure 

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
        Function { object_procedure = proc, object_arity = arity }
           | arity == -1 || arity == length args -> 
                callProcedure proc args 
           -- XXX should be raise of arity, typeError exception
           | otherwise -> raise typeError 
        Type { object_constructor = proc } -> callProcedure proc args
        -- XXX should try to find "__call__" attribute on object
        other -> raise typeError 

callProcedure :: Procedure -> [Object] -> Eval Object
callProcedure proc args = 
   callCC $ \ret -> do 
      push $ ProcedureCall ret
      proc args 

tailCall :: Object -> [Object] -> Eval Object 
tailCall obj args = do
    case obj of 
        Function { object_procedure = proc, object_arity = arity }
           | arity == -1 || arity == length args -> proc args
           | otherwise -> raise typeError 
        Type { object_constructor = proc } -> proc args
        -- XXX should try to find "__call__" attribute on object
        other -> raise typeError 

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

for :: ObjectRef -> Object -> Eval Object -> Eval Object
for var exp body = forElse var exp body pass 

forElse :: ObjectRef -> Object -> Eval Object -> Eval Object -> Eval Object
forElse var expObj suite1 suite2 = do
   iterObj <- callMethod expObj $(hashedStr "__iter__") [] -- this could be specialised
   cond <- newIORef true
   let tryBlock = do nextObj <- callMethod iterObj $(hashedStr "__next__") [] -- this could be specialised
                     writeIORef var nextObj
                     suite1
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
subs obj subscript = callMethod obj $(hashedStr "__getitem__") [subscript]

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
   callCC $ \afterTry -> do
      push (ExceptionHandler 
              (Just $ \obj -> do
                   handler obj 
                   afterTry none) 
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
   where
   -- XXX fixme, this is not correct
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
     -- putStrLn ("Uncaught exception: " ++ str)
     -- printObject exceptionObj 
     -- exitWith uncaughtExceptionError
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
      writeIORef (object_continuation genObj) (raise stopIteration)
      pop >> raise exceptionObj
   handleFrame exceptionObj other = do
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
            action
            BELCH("raising exception")
            raise stopIteration 
   ret result

def :: ObjectRef -> Arity -> Object -> ([ObjectRef] -> Eval Object) -> Eval Object 
def ident arity docString fun = do
   let procedureObj = function arity closure
   setattr procedureObj docName docString
   writeIORef ident procedureObj
   return none 
   where
   closure :: Procedure 
   closure params = do
      argsRefs <- mapM newIORef params 
      fun argsRefs 

lambda :: Arity -> ([ObjectRef] -> Eval Object) -> Eval Object
lambda arity fun = 
   return $ function arity closure 
   where
   closure :: Procedure 
   closure params = do
      argsRefs <- mapM newIORef params 
      fun argsRefs

mkGenerator :: Eval Object -> Eval Object
mkGenerator cont = do
   generatorObj <- generator cont
   ret generatorObj

printObject :: Object -> Eval () 
printObject obj = do
   str <- showObject obj
   putStr str 

showObject :: Object -> Eval String
-- XXX this should really choose the right quotes based on the content of the string.
showObject obj@(String {}) = return ("'" ++ object_string obj ++ "'")
showObject obj = object_string <$> callSpecialMethod obj strName []
