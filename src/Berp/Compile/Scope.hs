{-# LANGUAGE TypeSynonymInstances #-}
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
   ) where

import Language.Python.Common.AST as Py
import Data.Set as Set
import Berp.Compile.IdentString (IdentString, ToIdentString (..), IdentString (..))
import Data.List (intersperse, foldl')
import Data.Monoid
import Berp.Compile.PySyntaxUtils (varToString, paramIdent)

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

type NestingLevel = Int

type VarSet = Set IdentString

-- XXX Incomplete
assignTargets :: [ExprSpan] -> VarSet
assignTargets = foldl' addTarget mempty
   where
   addTarget :: VarSet -> ExprSpan -> VarSet
   addTarget set expr = Set.union (exprVars expr) set
   exprVars :: ExprSpan -> VarSet
   exprVars var@(Var {}) = Set.singleton $ varToString var
   exprVars list@(List {}) = Set.unions $ Prelude.map exprVars $ list_exprs list
   exprVars tuple@(Tuple {}) = Set.unions $ Prelude.map exprVars $ tuple_exprs tuple
   exprVars (Paren { paren_expr = expr }) = exprVars expr
   exprVars _other = Set.empty

topBindings :: SuiteSpan -> Either String Scope
topBindings stmts
   | not $ Set.null nonLocals
        = Left $ "These variables are declared nonlocal at the top level: " ++ prettyVarSet nonLocals
   | otherwise = Right $ emptyScope { localVars = locals, globalVars = globals }
   where
   (locals, nonLocals, globals) = termBindings stmts

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
   theseGlobals = globals varBindings
   theseNonLocals = nonlocals varBindings
   theseLocals = (Set.\\) ((Set.\\) (assigned varBindings) theseGlobals) theseNonLocals

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
prettyVarSet = concat . intersperse "," . Prelude.map fromIdentString . Set.toList

data BindingSets
   = BindingSets { assigned :: VarSet, nonlocals :: VarSet, globals :: VarSet }

instance Monoid BindingSets where
   mempty = BindingSets { assigned = Set.empty, nonlocals = Set.empty, globals = Set.empty }
   mappend x y
      = BindingSets
        { assigned = assigned x `mappend` assigned y
        , nonlocals = nonlocals x `mappend` nonlocals y
        , globals = globals x `mappend` globals y }

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
   -- The function name _is_ collected.
   definedVars (Fun { fun_name = n })
      = mempty { assigned = Set.singleton $ toIdentString n }
   definedVars (Class { class_name = ident, class_body = _b })
      = mempty { assigned = Set.singleton $ toIdentString ident } -- `mappend` definedVars b 
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
   -- definedVars ( StmtExpr { stmt_expr = e }) = definedVars e
   definedVars ( StmtExpr {} ) = mempty
   definedVars _other = mempty

-- We don't need to look inside expressions because:
-- 1) Generally expressions do not introduce new variables,
--    except for lambdas and comprehensions. 
--    Lambdas are easy to handle because all the variables
--    they introduce are mentioned in the head. Comprehensions
--    are compiled by desugaring to statements, so the rules for
--    statements suffice.
-- 2) Expressions do not contain nested statements.
instance DefinedVars ExprSpan where
{-
   definedVars ( Generator { gen_comprehension = comp })
      = definedVars comp
   definedVars ( ListComp { list_comprehension = comp })
      = definedVars comp
   definedVars ( DictComp { dict_comprehension = comp })
      = definedVars comp
-}
   definedVars _other = mempty

{-
instance DefinedVars (ComprehensionSpan e) where
   definedVars comp = definedVars $ comprehension_for comp

instance DefinedVars CompForSpan where
   definedVars (CompFor { comp_for_exprs = es, comp_in_expr = e, comp_for_iter = i })
      = mempty { assigned = assignTargets es} `mappend` definedVars e `mappend` definedVars i

instance DefinedVars CompIterSpan where
   definedVars (IterFor { comp_iter_for = i }) = definedVars i
   definedVars (IterIf { comp_iter_if = i }) = definedVars i

instance DefinedVars CompIfSpan where
   definedVars (CompIf { comp_if = e, comp_if_iter = i })
      = definedVars e `mappend` definedVars i
-}
