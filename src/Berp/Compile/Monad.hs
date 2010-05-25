{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.Monad
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Monad code to support the compiler.
--
-----------------------------------------------------------------------------

module Berp.Compile.Monad where

import Prelude hiding (catch)
import Control.Monad.State.Strict as State hiding (State) 
import Language.Python.Common.AST 
import Language.Python.Common.SrcLocation
import Language.Haskell.Exts.Syntax (Stmt, Name) 
import Language.Haskell.Exts.Build (name) 
import Data.Set
import Data.Either (either)
import Control.Applicative hiding (empty)
import Berp.Compile.VarSet (VarSet)
import Berp.Compile.IdentString (IdentString (..))
import qualified MonadUtils (MonadIO (..))
import Exception (ExceptionMonad (..))
import Control.Exception.Extensible (block, unblock, catch)

data State = State { unique :: !Integer, seen_yield :: !Bool, scope :: Scope }

data Scope 
   = Scope 
     { localVars :: !VarSet     -- local to a block (not params)
     , paramVars :: !VarSet     -- bound in the parameters of the innermost enclosing function 
     , globalVars :: !VarSet    -- declared as "global" in the source 
     , enclosingVars :: !VarSet -- in scope enclosing a block, but not global
     , nestingLevel :: !NestingLevel
     }
     deriving (Show)

-- This must remain empty, because it is used to create new scopes.
emptyScope :: Scope 
emptyScope
   = Scope 
     { localVars = empty
     , paramVars = empty
     , globalVars = empty
     , enclosingVars = empty 
     , nestingLevel = 0
     }

getScope :: Compile Scope
getScope = gets scope

setScope :: Scope -> Compile ()
setScope s = modify $ \state -> state { scope = s }

initState :: State
initState = State { unique = 0, seen_yield = False, scope = emptyScope } 

type NestingLevel = Int

newtype Compile a 
   = Compile (StateT State IO a)
   deriving (Monad, Functor, MonadIO, ExceptionMonad, Applicative)

-- the MonadState instance can't be derived by GHC
-- because we're using the monads-tf (type families), and they 
-- cause the GHC deriver to choke. Sigh.

instance MonadState Compile where
   type (StateType Compile) = State
   get = Compile get
   put s = Compile $ put s

-- needed to use Compile inside the GhcT monad transformer.
instance MonadUtils.MonadIO Compile where
   liftIO = State.liftIO 

-- needed to use Compile inside the GhcT monad transformer.
-- instance (Monoid w) => ExceptionMonad (RWST r w s IO) where
instance ExceptionMonad (StateT s IO) where
    gcatch m f = StateT $ \s -> runStateT m s
                           `catch` \e -> runStateT (f e) s
    gblock       = mapStateT block
    gunblock     = mapStateT unblock

runCompileMonad :: Compile a -> IO a
runCompileMonad (Compile comp) = 
   evalStateT comp initState 

getSeenYield :: Compile Bool
getSeenYield = gets seen_yield

unSetSeenYield :: Compile ()
unSetSeenYield = setSeenYield False 

setSeenYield :: Bool -> Compile ()
setSeenYield b = modify $ \state -> state { seen_yield = b }

isTopLevel :: Compile Bool
isTopLevel = gets ((== 1) . nestingLevel . scope)

incNestingLevel :: Scope -> Scope 
incNestingLevel scope = scope { nestingLevel = nestingLevel scope + 1 } 

freshVarRaw :: Compile String
freshVarRaw = do
   u <- gets unique
   modify $ \state -> state { unique = u + 1 } 
   return ("_t_" ++ show u)

freshHaskellVar :: Compile Name
freshHaskellVar = name <$> freshVarRaw 

freshPythonVar :: Compile IdentSpan
freshPythonVar = do
   rawVar <- freshVarRaw
   return $ Ident { ident_string = rawVar, ident_annot = SpanEmpty }

checkEither :: Either String b -> Compile b
checkEither (Left e) = fail e
checkEither (Right v) = return v
