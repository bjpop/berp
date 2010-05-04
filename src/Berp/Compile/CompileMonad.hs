{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}

module Berp.Compile.CompileMonad where

import Prelude hiding (catch)
import Control.Monad.RWS as RWS -- should we use the Strict version?
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

data State = State { unique :: !Integer, seen_yield :: !Bool }

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
getScope = ask 

initState :: State
initState = State { unique = 0, seen_yield = False } 

type NestingLevel = Int

newtype Compile a 
   = Compile (RWST Scope () State IO a)
   deriving (Monad, Functor, MonadIO, ExceptionMonad, Applicative)

-- the MonadReader and MonadState instances can't be derived by GHC
-- because we're using the monads-tf (type families), and they 
-- cause the GHC deriver to choke. Sigh.

instance MonadReader Compile where
   type (EnvType Compile) = Scope
   ask = Compile ask  
   local f (Compile m) = Compile $ local f m

instance MonadState Compile where
   type (StateType Compile) = State
   get = Compile get
   put s = Compile $ put s

-- needed to use Compile inside the GhcT monad transformer.
instance MonadUtils.MonadIO Compile where
   liftIO = RWS.liftIO 

-- needed to use Compile inside the GhcT monad transformer.
instance (Monoid w) => ExceptionMonad (RWST r w s IO) where
    gcatch m f = RWST $ \r s -> runRWST m r s
                           `catch` \e -> runRWST (f e) r s
    gblock       = mapRWST block
    gunblock     = mapRWST unblock

runCompileMonad :: Compile a -> IO a
runCompileMonad (Compile comp) = 
   fst <$> evalRWST comp emptyScope initState 

getSeenYield :: Compile Bool
getSeenYield = gets seen_yield

unSetSeenYield :: Compile ()
unSetSeenYield = setSeenYield False 

setSeenYield :: Bool -> Compile ()
setSeenYield b = 
   modify $ \state -> state { seen_yield = b }

isTopLevel :: Compile Bool
isTopLevel = asks ((== 1) . nestingLevel)

incNestingLevel :: Scope -> Scope 
incNestingLevel scope = 
   scope { nestingLevel = nestingLevel scope + 1 } 

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
