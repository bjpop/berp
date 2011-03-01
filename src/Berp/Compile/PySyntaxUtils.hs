{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.PySyntaxUtils
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Utilities for processing Python syntax.
--
-----------------------------------------------------------------------------

module Berp.Compile.PySyntaxUtils where

import Language.Python.Common.AST as Py
import Language.Python.Common.SrcLocation ( SrcSpan (..) )
import qualified Data.Set as Set
import Data.List (intersperse, foldl')
import Data.Monoid
import Berp.Compile.Monad (Scope (..), emptyScope)
import Berp.Compile.IdentString (ToIdentString (..), IdentString (..))
import Berp.Compile.VarSet (VarSet)

data InterpreterStmt = InterpreterStmt Py.SuiteSpan

isEmptySuite :: Suite a -> Bool
isEmptySuite [] = True
isEmptySuite _ = False

-- Incomplete
assignTargets :: [ExprSpan] -> VarSet 
assignTargets = foldl' addTarget mempty
   where
   addTarget :: VarSet -> ExprSpan -> VarSet
   -- addTarget set var@(Var {}) = Set.insert (varToString var) set
   addTarget set expr = Set.union (exprVars expr) set
   exprVars :: ExprSpan -> VarSet
   exprVars var@(Var {}) = Set.singleton (varToString var)
   exprVars list@(List {}) = Set.unions $ Prelude.map exprVars $ list_exprs list
   exprVars tuple@(Tuple {}) = Set.unions $ Prelude.map exprVars $ tuple_exprs tuple 
   exprVars (Paren { paren_expr = expr }) = exprVars expr
   exprVars _other = Set.empty
   

varToString :: Show a => Expr a -> IdentString
varToString v@(Var {}) = toIdentString $ var_ident v
varToString other = error $ "fatal error: varToString called on non variable argument" ++ show other

{-
toIdentString :: Ident a -> IdentString
toIdentString (Ident { ident_string = name }) = IdentString name
-}

paramIdent :: Parameter a -> Ident a
paramIdent = param_name 

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

-- (currently) variables are not atomic because they are always mutable
-- and reading a variable is an effect. If we add single binding variables
-- then they would be atomic.
isAtomicExpr :: Py.ExprSpan -> Bool
isAtomicExpr (Py.Strings {}) = True
isAtomicExpr (Py.Bool {}) = True
isAtomicExpr (Py.Int {}) = True
isAtomicExpr (Py.Float {}) = True
isAtomicExpr (Py.Imaginary {}) = True
isAtomicExpr (Py.Tuple {}) = True
-- isAtomicExpr (Py.List {}) = True
isAtomicExpr (Py.Paren { paren_expr = e }) = isAtomicExpr e
isAtomicExpr (Py.None {}) = True
isAtomicExpr _other = False

ident :: String -> IdentSpan
ident s = Ident { ident_string = s, ident_annot = SpanEmpty }
var :: IdentSpan -> ExprSpan
var ident = Var { var_ident = ident, expr_annot = SpanEmpty }
list :: [ExprSpan] -> ExprSpan
list es = List { list_exprs = es, expr_annot = SpanEmpty }
set :: [ExprSpan] -> ExprSpan
set es = Set { set_exprs = es, expr_annot = SpanEmpty }
dict :: [(ExprSpan, ExprSpan)] -> ExprSpan
dict es = Dictionary { dict_mappings = es, expr_annot = SpanEmpty }
tuple :: [ExprSpan] -> ExprSpan
tuple es = Py.Tuple { tuple_exprs = es, expr_annot = SpanEmpty }
call :: ExprSpan -> [ExprSpan] -> ExprSpan
call fun args =
   Call { call_fun = fun, call_args = map argExpr args, expr_annot = SpanEmpty }
binOp :: OpSpan -> ExprSpan -> ExprSpan -> ExprSpan
binOp op lhs rhs =
   BinaryOp { operator = op
            , left_op_arg = lhs
            , right_op_arg = rhs
            , expr_annot = SpanEmpty }
subscript :: ExprSpan -> ExprSpan -> ExprSpan
subscript e1 e2 = Subscript { subscriptee = e1, subscript_expr = e2, expr_annot = SpanEmpty }
yield :: ExprSpan -> ExprSpan
yield e = Yield { yield_expr = Just e, expr_annot = SpanEmpty }
argExpr :: ExprSpan -> ArgumentSpan
argExpr e = ArgExpr { arg_expr = e, arg_annot = SpanEmpty }
pass :: StatementSpan
pass = Pass { stmt_annot = SpanEmpty }
assign :: ExprSpan -> ExprSpan -> StatementSpan
assign lhs rhs =
   Assign { assign_to = [lhs]
          , assign_expr = rhs
          , stmt_annot = SpanEmpty }
stmtExpr :: ExprSpan -> StatementSpan
stmtExpr e = StmtExpr { stmt_expr = e, stmt_annot = SpanEmpty }
conditional :: [(ExprSpan, SuiteSpan)] -> SuiteSpan -> StatementSpan
conditional conds elsePart =
   Conditional { cond_guards = conds, cond_else = elsePart, stmt_annot = SpanEmpty }
for :: [ExprSpan] -> ExprSpan -> SuiteSpan -> StatementSpan
for targets gen body
   = For { for_targets = targets, for_generator = gen, for_body = body, for_else = [], stmt_annot = SpanEmpty }
dot :: OpSpan
dot = Dot { op_annot = SpanEmpty }
def :: IdentSpan -> [ParameterSpan] -> SuiteSpan -> StatementSpan
def name args body =
   Fun { fun_name = name
       , fun_args = args
       , fun_result_annotation = Nothing
       , fun_body = body
       , stmt_annot = SpanEmpty
       }
