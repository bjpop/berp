{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.Scope
-- Copyright   : (c) 2011 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Variable scope for compiler.
--
-----------------------------------------------------------------------------

module Berp.Compile.Scope
   ( Scope (..)
   , emptyScope
   , NestingLevel
   , VarSet
   , topBindings
   , funBindings
   , prettyVarSet
   , isLocal
   , isGlobal
   ) where

import Language.Python.Common.AST as Py
import Data.Set as Set
import Berp.Compile.IdentString (IdentString, ToIdentString (..), IdentString (..))
import qualified Data.List as L (intersperse, foldl')
import Data.Monoid
import Berp.Compile.PySyntaxUtils (varToString, paramIdent)

data Scope
   = Scope
     { localVars :: !VarSet          -- local to a block (not params)
       -- we keep params separate because we want to include them in enclosing vars
       -- but we want to distinguish them from locals.
     , paramVars :: !VarSet          -- bound in the parameters of the innermost enclosing function
     , globalVars :: !VarSet         -- declared as "global" in the source
     , enclosingVars :: !VarSet      -- in scope enclosing a block, but not global
     , nestingLevel :: !NestingLevel -- how deep are we in the scope?
     }
     deriving (Show)

-- a variable is global iff it is explicitly declared so in this scope, or
-- it is neither local nor defined in an enclosing scope nor a parameter of an enclosing function
isGlobal :: IdentString -> Scope -> Bool
isGlobal ident scope =
   ident `member` (globalVars scope) ||
   (ident `notMember` (localVars scope) &&
    ident `notMember` (enclosingVars scope) &&
    ident `notMember` (paramVars scope))

-- a variable is local if it is in the localVars
isLocal :: IdentString -> Scope -> Bool
isLocal ident scope = ident `member` localVars scope

type NestingLevel = Int

-- this is the level of the top scope in a module (and the interpreter REPL scope).
outermostNestingLevel :: NestingLevel
outermostNestingLevel = 0

-- This must remain empty, because it is used to create new scopes.
emptyScope :: Scope
emptyScope
   = Scope
     { localVars = empty
     , paramVars = empty
     , globalVars = empty
     , enclosingVars = empty
     , nestingLevel = outermostNestingLevel
     }

type VarSet = Set IdentString

-- Collect all the variables which are assigned to in a list of expressions (patterns).
-- XXX Incomplete
assignTargets :: [ExprSpan] -> VarSet
assignTargets = L.foldl' addTarget mempty
   where
   addTarget :: VarSet -> ExprSpan -> VarSet
   addTarget set expr = Set.union (exprVars expr) set
   exprVars :: ExprSpan -> VarSet
   exprVars var@(Var {}) = Set.singleton $ varToString var
   exprVars list@(List {}) = Set.unions $ Prelude.map exprVars $ list_exprs list
   exprVars tuple@(Tuple {}) = Set.unions $ Prelude.map exprVars $ tuple_exprs tuple
   exprVars (Paren { paren_expr = expr }) = exprVars expr
   exprVars _other = Set.empty

-- XXX I think this should now just make empty scopes.
-- top level is somewhat special because
-- all assigned variables are global, there should be no "nonlocal" declarations and "global"
-- declarations are pointless. There is also no enclosing scope. Also variables defined at
-- the top level don't need to be treated as enclosing variables for nested scopes because
-- they are global (they aren't in the local closure, only the global closure - which is treated
-- specially).
topBindings :: Scope
topBindings = emptyScope
{-
topBindings :: SuiteSpan -> Either String Scope
topBindings stmts
   | not $ Set.null nonLocals
        = Left $ "These variables are declared nonlocal at the top level: " ++ prettyVarSet nonLocals
   | otherwise = Right $ emptyScope { localVars = locals, globalVars = globals }
   where
   (locals, nonLocals, globals) = termBindings stmts
-}

funBindings :: DefinedVars t => [ParameterSpan] -> t -> Either String Scope
funBindings params term
   = case allDisjoint paramIdents nonLocals globals of
        Nothing -> Right $ emptyScope
           { localVars = (Set.\\) locals paramIdents
           , paramVars = paramIdents
           , globalVars = globals }
        Just error -> Left error
   where
   paramIdents = Set.fromList $ Prelude.map (toIdentString . paramIdent) params
   (locals, nonLocals, globals) = termBindings term

termBindings :: DefinedVars t => t -> (VarSet, VarSet, VarSet)
termBindings term
   = (theseLocals, theseNonLocals, theseGlobals)
   where
   varBindings = definedVars term
   -- here we consider globals and nonlocals as those variables explicitly declared such
   -- this is not necessarily the full set of globals on nonlocals, but it provides sufficient
   -- information for compilation.
   theseGlobals = globals varBindings
   theseNonLocals = nonlocals varBindings
   -- local variables are those assigned variables which are not declared nonlocal or global
   theseLocals = (Set.\\) ((Set.\\) (assigned varBindings) theseGlobals) theseNonLocals

-- check that the parameters of a function, the nonlocals and the globals are all disjoint.
allDisjoint :: VarSet -> VarSet -> VarSet -> Maybe String
allDisjoint params nonlocals globals
   = if not (Set.null ps_ns)
        then Just $ "These variables are parameters and declared nonlocal: " ++ prettyVarSet ps_ns 
        else if not (Set.null ps_gs)
                then Just $ "These variables are parameters and declared global: " ++ prettyVarSet ps_gs
                else if not (Set.null ns_gs)
                        then Just $ "These variables are declared nonlocal and global: " ++ prettyVarSet ns_gs
                        else Nothing
   where
   ps_ns = params `Set.intersection` nonlocals
   ps_gs = params `Set.intersection` globals
   ns_gs = nonlocals `Set.intersection` globals

prettyVarSet :: VarSet -> String
prettyVarSet = concat . L.intersperse "," . Prelude.map fromIdentString . Set.toList

data BindingSets =
   BindingSets
   { assigned :: VarSet   -- variables assigned to in this scope
   , nonlocals :: VarSet  -- variables declared nonlocal in this scope
   , globals :: VarSet    -- variables declared global in this scope
   }

instance Monoid BindingSets where
   mempty = BindingSets { assigned = Set.empty, nonlocals = Set.empty, globals = Set.empty }
   mappend x y
      = BindingSets
        { assigned = assigned x `mappend` assigned y
        , nonlocals = nonlocals x `mappend` nonlocals y
        , globals = globals x `mappend` globals y }

-- determine the set of variables which are either assigned to or explicitly declared global or
-- nonlocal in the current scope.
class DefinedVars t where
   definedVars :: t -> BindingSets

instance DefinedVars t => DefinedVars [t] where
   definedVars = mconcat . Prelude.map definedVars

instance (DefinedVars t1, DefinedVars t2) => DefinedVars (t1, t2) where
   definedVars (x,y) = definedVars x `mappend` definedVars y

instance DefinedVars a => DefinedVars (Maybe a) where
   definedVars Nothing = mempty
   definedVars (Just x) = definedVars x

-- XXX Do we need to search inside nested expressions? I don't think so.
instance DefinedVars (StatementSpan) where
   definedVars (While { while_body = b, while_else = e })
      = definedVars b `mappend` definedVars e
   definedVars (For { for_targets = t, for_body = b, for_else = e })
      = mempty { assigned = assignTargets t} `mappend` definedVars b `mappend` definedVars e
   -- Any definedVars made inside a function body are not collected.
   -- The function name _is_ collected, because it is assigned in the current scope,
   -- likewise for the class name.
   definedVars (Fun { fun_name = n })
      = mempty { assigned = Set.singleton $ toIdentString n }
   definedVars (Class { class_name = ident, class_body = _b })
      = mempty { assigned = Set.singleton $ toIdentString ident }
   definedVars (Conditional { cond_guards = g, cond_else = e })
      = definedVars g `mappend` definedVars e
   definedVars (Assign { assign_to = t })
      = mempty { assigned = assignTargets t }
   definedVars (Decorated { decorated_def = d })
       = definedVars d
   definedVars (Try { try_body = b, try_else = e, try_finally = f})
       = definedVars [b,e,f]
   definedVars (With { with_body = b })
      = definedVars b
   definedVars (Global { global_vars = idents })
      = mempty { globals = Set.fromList $ Prelude.map toIdentString idents }
   definedVars (NonLocal { nonLocal_vars = idents })
      = mempty { nonlocals = Set.fromList $ Prelude.map toIdentString idents }
   definedVars ( StmtExpr {} ) = mempty
   definedVars _other = mempty

-- We don't need to look inside expressions (or anything inside them) because:
-- 1) Generally expressions do not introduce new variables,
--    except for lambdas and comprehensions. 
--    Lambdas are easy to handle because all the variables
--    they introduce are mentioned in the head. Comprehensions
--    are compiled by desugaring to statements, so the rules for
--    statements suffice.
-- 2) Expressions do not contain nested statements.
instance DefinedVars ExprSpan where
   definedVars _other = mempty
