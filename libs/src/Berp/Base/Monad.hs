-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Monad
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Support for the Eval monad.
--
-----------------------------------------------------------------------------

module Berp.Base.Monad
   (runEval, updateModuleCache, lookupModuleCache, getUnique) where

import Control.Concurrent.MVar (MVar)
import Control.Applicative ((<$>))
import Control.Monad.State.Strict (evalStateT, gets, modify)
import Control.Monad.Cont (runContT)
import qualified Data.Map as Map (lookup, insert)
import Berp.Base.Unique (Unique)
import Berp.Base.SemanticTypes
   (Object (..), Eval, EvalState (..))

runEval :: EvalState -> Eval Object -> IO Object
runEval state comp = runContT (evalStateT comp state) return

lookupModuleCache :: String -> Eval (Maybe Object)
lookupModuleCache moduleName =
   Map.lookup moduleName <$> gets state_moduleCache

-- XXX is there a lazy leak here in the update?
updateModuleCache :: String -> Object -> Eval ()
updateModuleCache moduleName object = do
   oldCache <- gets state_moduleCache
   modify $ \state -> state { state_moduleCache = Map.insert moduleName object oldCache }

getUnique :: Eval (MVar Unique)
getUnique = gets state_unique
