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
import Berp.Compile.IdentString (ToIdentString (..), IdentString (..))

data InterpreterStmt = InterpreterStmt Py.SuiteSpan

isEmptySuite :: Suite a -> Bool
isEmptySuite [] = True
isEmptySuite _ = False

varToString :: Show a => Expr a -> IdentString
varToString v@(Var {}) = toIdentString $ var_ident v
varToString other = error $ "fatal error: varToString called on non variable argument" ++ show other

paramIdent :: Parameter a -> Ident a
paramIdent = param_name

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
