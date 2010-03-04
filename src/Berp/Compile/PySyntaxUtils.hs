{-# LANGUAGE TypeSynonymInstances #-}
module Berp.Compile.PySyntaxUtils where

-- import Language.Python.Version3.Syntax.AST as Py
import Language.Python.Common.AST as Py
import Data.Set as Set 
import Berp.Compile.Utils
import Data.List (intersperse, foldl')
import Data.Monoid
import Berp.Compile.CompileMonad (Scope (..), emptyScope)
import Berp.Compile.IdentString (ToIdentString (..), IdentString (..))
import Berp.Compile.VarSet (VarSet)

isEmptySuite :: Suite a -> Bool
isEmptySuite [] = True
isEmptySuite _ = False

-- Incomplete
assignTargets :: [ExprSpan] -> VarSet 
assignTargets = foldl' addTarget mempty
   where
   addTarget :: VarSet -> ExprSpan -> VarSet
   addTarget set var@(Var {}) = Set.insert (varToString var) set
   addTarget set _other = set

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
           { localVars = locals \\ paramIdents
           , paramVars = paramIdents 
           , globalVars = globals } 
        Just error -> Left error
   where
   paramIdents = fromList $ Prelude.map (toIdentString . paramIdent) params 
   (locals, nonLocals, globals) = termBindings term 

termBindings :: DefinedVars t => t -> (VarSet, VarSet, VarSet)
termBindings term 
   = (theseLocals, theseNonLocals, theseGlobals) 
   where
   varBindings = definedVars term 
   theseGlobals = globals varBindings
   theseNonLocals = nonlocals varBindings
   theseLocals = ((assigned varBindings \\ theseGlobals) \\ theseNonLocals) 

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
   ps_ns = params `intersection` nonlocals
   ps_gs = params `intersection` globals
   ns_gs = nonlocals `intersection` globals 

prettyVarSet :: VarSet -> String
prettyVarSet = concat . intersperse "," . Prelude.map fromIdentString . toList 

data BindingSets
   = BindingSets { assigned :: VarSet, nonlocals :: VarSet, globals :: VarSet }

instance Monoid BindingSets where
   mempty = BindingSets { assigned = empty, nonlocals = empty, globals = empty }
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

instance DefinedVars (StatementSpan) where
   definedVars (While { while_body = b, while_else = e })
      = definedVars b `mappend` definedVars e 
   definedVars (For { for_body = b, for_else = e })
      = definedVars b `mappend` definedVars e 
   -- Any definedVars made inside a function body are not collected.
   -- The function name _is_ collected.
   definedVars (Fun { fun_name = n })
      = mempty { assigned = singleton $ toIdentString n }
   definedVars (Class { class_name = ident, class_body = b })
      = mempty { assigned = singleton $ toIdentString ident } -- `mappend` definedVars b 
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
      = mempty { globals = fromList $ Prelude.map toIdentString idents } 
   definedVars (NonLocal { nonLocal_vars = idents })
      = mempty { nonlocals = fromList $ Prelude.map toIdentString idents }
   definedVars other = mempty

instance DefinedVars (Expr a) where
   definedVars = mempty 

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
isAtomicExpr (Py.List {}) = True
isAtomicExpr (Py.Paren { paren_expr = e }) = isAtomicExpr e
isAtomicExpr (Py.None {}) = True
isAtomicExpr other = False

-- True if the expression has no observable effect when treated as a statement
-- conservative approximation.
isPureExpr :: Py.Expr a -> Bool
isPureExpr (Py.Var {}) = True
isPureExpr (Py.Strings {}) = True
isPureExpr (Py.Bool {}) = True
isPureExpr (Py.Int {}) = True
isPureExpr (Py.Imaginary {}) = True
isPureExpr (Py.Tuple { tuple_exprs = es }) = all isPureExpr es 
isPureExpr (Py.List { list_exprs = es }) = all isPureExpr es 
isPureExpr (Py.Lambda {}) = True
isPureExpr (Py.None {}) = True
isPureExpr (Py.Paren { paren_expr = e }) = isPureExpr e
