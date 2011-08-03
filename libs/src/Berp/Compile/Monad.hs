{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

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
-- import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (MonadState (..))
import Language.Python.Common.SrcLocation
import Language.Python.Common.AST (IdentSpan, Ident (..))
import Language.Haskell.Exts.Syntax (Name)
import Language.Haskell.Exts.Build (name)
import Control.Applicative hiding (empty)
-- import Control.Monad
-- import qualified MonadUtils (MonadIO (..))
-- import Exception (ExceptionMonad (..))
-- import Control.Exception.Extensible (block, unblock, catch)
import Control.Monad.CatchIO as CatchIO (MonadCatchIO (..))
import qualified Data.Set as Set (Set, insert, empty)
import Berp.Compile.Scope (Scope (..), emptyScope)

type ImportSet = Set.Set String

data State
    = State
      { unique :: !Integer
      , seenYield :: !Bool
      , scope :: Scope
      , importModules :: !ImportSet
      }

getImports :: Compile ImportSet
getImports = gets importModules

setImports :: ImportSet -> Compile ()
setImports is = modify $ \state -> state { importModules = is }

addImport :: String -> Compile ()
addImport i = do
   imports <- getImports
   setImports $ Set.insert i imports

withScope :: (Scope -> b) -> Compile b
withScope f = do
   s <- getScope
   return $ f s

getScope :: Compile Scope
getScope = gets scope

setScope :: Scope -> Compile ()
setScope s = modify $ \state -> state { scope = s }

initState :: State
initState
   = State
     { unique = 0
     , seenYield = False
     , scope = emptyScope
     , importModules = Set.empty
     }

newtype Compile a
   = Compile (StateT State IO a)
   -- deriving (Monad, Functor, MonadIO, ExceptionMonad, Applicative)
   deriving (Monad, Functor, MonadIO, Applicative, MonadCatchIO)

-- the MonadState instance can't be derived by GHC
-- because we're using the monads-tf (type families), and they 
-- cause the GHC deriver to choke. Sigh.

-- annoyingly the CatchIO module does not define this instance for the strict
-- state monad, only the lazy one.
instance MonadCatchIO m => MonadCatchIO (StateT s m) where
    m `catch` f = StateT $ \s -> (runStateT m s)
                                   `CatchIO.catch` (\e -> runStateT (f e) s)
    block       = mapStateT block
    unblock     = mapStateT unblock

instance MonadState State Compile where
   -- type (StateType Compile) = State
   get = Compile get
   put s = Compile $ put s



-- needed to use Compile inside the GhcT monad transformer.
{-
instance MonadIO Compile where
   liftIO = State.liftIO
-}

-- needed to use Compile inside the GhcT monad transformer.
-- instance (Monoid w) => ExceptionMonad (RWST r w s IO) where
{-
instance ExceptionMonad (StateT s IO) where
    gcatch m f = StateT $ \s -> runStateT m s
                           `catch` \e -> runStateT (f e) s
    gblock       = mapStateT block
    gunblock     = mapStateT unblock
-}

runCompileMonad :: Compile a -> IO a
runCompileMonad (Compile comp) =
   evalStateT comp initState

getSeenYield :: Compile Bool
getSeenYield = gets seenYield

unSetSeenYield :: Compile ()
unSetSeenYield = setSeenYield False

setSeenYield :: Bool -> Compile ()
setSeenYield b = modify $ \state -> state { seenYield = b }

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
