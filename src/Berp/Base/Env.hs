module Berp.Base.Env 
   (VarEnv, emptyVarEnv,
   lookupVarEnv, updateVarEnv, updateGlobalEnv,
   dumpVarEnv, dumpEnv, varEnvFromList, methodsFromList ) where

import Data.Map as Map
import Data.IORef (readIORef, writeIORef, newIORef)
import Control.Monad.Trans (liftIO)
import Control.Monad.State 
import Berp.Base.Ident (Ident)
import Berp.Base.SemanticTypes (ObjectRef, Eval, EvalState (..), VarEnv, Procedure, Arity)
import {-# SOURCE #-} Berp.Base.StdTypes.Function (function)
import Berp.Base.Mangle (mangle)

emptyVarEnv :: IO VarEnv
emptyVarEnv = newIORef Map.empty

lookupVarEnv :: Ident -> VarEnv -> Eval (Maybe ObjectRef)
lookupVarEnv ident varEnv = do
    env <- liftIO $ readIORef varEnv
    Prelude.return $ Map.lookup ident env

updateVarEnv :: VarEnv -> Ident -> ObjectRef -> Eval ()
updateVarEnv varEnv ident ref = liftIO $ do
   env <- readIORef varEnv
   let newEnv = Map.insert ident ref env
   writeIORef varEnv newEnv

varEnvFromList :: [(Ident, ObjectRef)] -> IO VarEnv
varEnvFromList = newIORef . Map.fromList 

{-
updateLocalEnv :: Ident -> ObjectRef -> Eval ()
updateLocalEnv ident ref = do
   localsRef <- gets local_env
   updateVarEnv localsRef ident ref
-}

updateGlobalEnv :: Ident -> ObjectRef -> Eval ()
updateGlobalEnv ident ref = do
   -- globalsRef <- get
   globalsRef <- gets global_env 
   updateVarEnv globalsRef ident ref

dumpEnv :: String -> Eval ()
dumpEnv msg = do
   liftIO $ putStrLn $ "\n" ++ msg
{-
   locals <- asks local_env
   liftIO $ putStrLn "Local environment:"
   dumpVarEnv locals
   -- globals <- get
-}
   globals <- gets global_env
   liftIO $ putStrLn "Global environment:"
   dumpVarEnv globals

dumpVarEnv :: VarEnv -> Eval ()
dumpVarEnv varEnv = do
   env <- liftIO $ readIORef varEnv
   mapM_ dumpBinding $ Map.toList env
   where
   dumpBinding :: (Ident, ObjectRef) -> Eval ()
   dumpBinding (ident, ref) = do
       liftIO $ putStr $ ident ++ " = "
       object <- liftIO $ readIORef ref
       liftIO $ print object

methodsFromList :: [(Ident, Int, Procedure)] -> IO VarEnv
methodsFromList identsProcs = do
   identsObjectRefs <- mapM procToObj identsProcs
   newIORef $ fromList identsObjectRefs
   where
   procToObj :: (Ident, Int, Procedure) -> IO (Ident, ObjectRef)
   procToObj (ident, arity, proc) = do
      objRef <- topProcedureRef arity proc
      return (mangle ident, objRef)
   topProcedureRef :: Arity -> Procedure -> IO ObjectRef
   topProcedureRef arity proc = newIORef $ function arity proc 
