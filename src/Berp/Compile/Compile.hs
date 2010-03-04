{-# LANGUAGE PatternGuards, TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}
module Berp.Compile.Compile (compiler) where

import Prelude hiding (read, init, mapM)
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST as Py
import Data.Traversable
import Data.Foldable (foldrM)
import Control.Monad (when, liftM3) 
import Language.Haskell.Exts.Syntax as Hask
import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Pretty
import Control.Monad.RWS (get, put, gets, tell, listen, censor, local)
import Control.Applicative
import qualified Data.Set as Set
import Control.Monad hiding (mapM)
import Control.Monad.Trans (liftIO)
import qualified Berp.Compile.PrimName as Prim
import Berp.Compile.CompileMonad
import Berp.Compile.HsSyntaxUtils
import Berp.Compile.PySyntaxUtils
import Berp.Compile.Utils
import Berp.Base.Mangle (mangle)
import Berp.Base.Hash (Hash (..))
import Berp.Compile.VarSet (VarSet)
import Berp.Compile.IdentString (IdentString (..), ToIdentString (..), identString)

compiler :: Compilable a => a -> IO (CompileResult a)
compiler = runCompileMonad . compile 

class Compilable a where
   type CompileResult a :: *
   compile :: a -> Compile (CompileResult a)

instance Compilable a => Compilable [a] where
   type CompileResult [a] = [CompileResult a]
   compile = mapM compile

instance Compilable a => Compilable (Maybe a) where
   type CompileResult (Maybe a) = Maybe (CompileResult a)
   compile = mapM compile

instance Compilable ModuleSpan where
   type CompileResult ModuleSpan = Hask.Module
   compile (Py.Module suite) = do
      bindings <- checkEither $ topBindings suite
      stmts <- nestedScope bindings $ compileBlockDo $ Block suite 
      let init = initDecl stmts
      return $ Hask.Module bogusSrcLoc modName pragmas warnings exports imports 
                           [mainDecl, init] 
      where
      modName = ModuleName "Main"
      mainDecl :: Hask.Decl
      mainDecl = 
         patBind bogusSrcLoc mainPatName $ app Prim.start Prim.init
         where
         mainPatName = pvar $ name "main"
      initDecl :: Hask.Exp -> Hask.Decl
      initDecl = patBind bogusSrcLoc $ pvar Prim.initName
      pragmas = []
      warnings = Nothing
      exports = Nothing

instance Compilable StatementSpan where
   type (CompileResult StatementSpan) = [Stmt] 

   compile stmt@(Fun {fun_name = fun, fun_args = params, fun_body = body}) = do
      bindings <- checkEither $ funBindings params body
      compiledBody <- nestedScope bindings $ compileBlockDo $ Block body
      let args = Hask.PList $ map (identToMangledPatVar . paramIdent) params
      let lambda = lamE bogusSrcLoc [args] compiledBody
      returnStmt $ appFun Prim.def [identToMangledVar fun, intE $ fromIntegral $ length params, parens lambda]
   compile (Assign { assign_to = target, assign_expr = expr }) = 
      compileAssign (head target) expr
   compile (Conditional { cond_guards = guards, cond_else = elseBranch })
      | length guards == 1 && isEmptySuite elseBranch,
        (condExp, condSuite) <- head guards = do
           condVal <- compileExprBlock condExp
           condBody <- compileSuiteDo condSuite
           returnStmt $ appFun Prim.ifThen [parens condVal, parens condBody]
      | otherwise = do
           elseExp <- compileSuiteDo elseBranch
           condExp <- foldM compileGuard elseExp $ reverse guards
           returnStmt condExp
   compile (Return { return_expr = maybeExpr }) = do
      (stmts, compiledExpr) <- maybe (returnExp Prim.none) compileExprObject maybeExpr
      let newStmt = qualStmt $ app Prim.ret $ parens compiledExpr
      return (stmts ++ [newStmt])
   {- We could implement a small optimisation to avoid generating code for pure expressions
      which are used as statements. However this optimisation would not be warranted if
      we are generating code for an interactive environment.
   -}
   compile (StmtExpr { stmt_expr = expr }) = do
      (stmts, compiledExpr) <- compileExprComp expr
      let newStmt = qualStmt $ app Prim.stmt $ parens compiledExpr
      return (stmts ++ [newStmt])
   compile (While { while_cond = cond, while_body = body, while_else = elseSuite }) = do
      condVal <- compileExprBlock cond
      bodyExp <- compileSuiteDo body
      if isEmptySuite elseSuite
         then returnStmt $ appFun Prim.while [parens condVal, parens bodyExp]
         else do
            elseExp <- compileSuiteDo elseSuite
            returnStmt $ appFun Prim.whileElse [parens condVal, parens bodyExp, parens elseExp]
   -- XXX fixme, only supports one target
   compile (For { for_targets = [var@(Py.Var { })], for_generator = generator, for_body = body, for_else = elseSuite }) = do
      (varStmts, compiledVar) <- compile var 
      (generatorStmts, compiledGenerator) <- compileExprObject generator
      compiledBody <- compileSuiteDo body
      compiledElse <- compileSuiteDo elseSuite
      return (varStmts ++ generatorStmts ++ [qualStmt $ appFun Prim.for [compiledVar, compiledGenerator, parens compiledBody, parens compiledElse]])
{-
   compile stmt@(For {}) = do
      desugared <- desugarFor stmt
      concat <$> compile desugared 
-}
   compile (Pass {}) = returnStmt Prim.pass
   compile (NonLocal {}) = return [] 
   compile (Global {}) = return [] 
   compile (Class { class_name = ident, class_args = args, class_body = body }) = do
      bindings <- checkEither $ funBindings [] body
      -- XXX slightly dodgy since the syntax allows Argument types in class definitions but
      -- I'm not sure what their meaning is, or if it is just a case of the grammar over specifying
      -- the language
      (argsStmtss, compiledArgs) <- mapAndUnzipM (compileExprObject . arg_expr) args
      compiledBody <- nestedScope bindings $ compile $ Block body
      let locals = Set.toList $ localVars bindings
      attributes <- qualStmt <$> app Prim.pure <$> listE <$> mapM compile locals 
      let newStmt = qualStmt $ appFun Prim.klass
                       [ identToMangledVar ident
                       , listE compiledArgs 
                       , parens $ doBlock $ compiledBody ++ [attributes]]
      return (concat argsStmtss ++ [newStmt])
   compile (Try { try_body = body, try_excepts = handlers, try_else = elseSuite, try_finally = finally }) = do
      bodyExp <- compileSuiteDo body
      asName <- freshHaskellVar
      handlerExp <- compileHandlers (var asName) handlers
      let handlerLam = lamE bogusSrcLoc [pvar asName] handlerExp
      compiledElse <- compile elseSuite
      compiledFinally <- compile finally
      -- returnStmt $ appFun Prim.try [parens bodyExp, handlerLam]
      returnStmt $ mkTry (parens bodyExp) handlerLam (concat compiledElse) (concat compiledFinally)
   compile (Raise { raise_expr = RaiseV3 raised }) = 
      case raised of
         Nothing -> returnStmt Prim.reRaise
         Just (e, maybeFrom) ->
            case maybeFrom of
               Nothing -> do
                 (stmts, obj) <- compileExprObject e
                 let newStmt = qualStmt $ app Prim.raise obj 
                 return (stmts ++ [newStmt])
               Just fromExp -> do
                 (stmts1, obj1) <- compileExprObject e
                 (stmts2, obj2) <- compileExprObject fromExp
                 let newStmt = qualStmt $ appFun Prim.raiseFrom [obj1, obj2]
                 return (stmts1 ++ stmts2 ++ [newStmt])
   compile (Break {}) = returnStmt Prim.break
   compile (Continue {}) = returnStmt Prim.continue

mkTry :: Exp -> Exp -> [Stmt] -> [Stmt] -> Exp 
mkTry body handler elseSuite finally = 
   case (elseSuite, finally) of
      ([], []) -> appFun Prim.try [body, handler]
      (_:_, []) -> appFun Prim.tryElse [body, handler, elseBlock] 
      ([], _:_) -> appFun Prim.tryFinally [body, handler, finallyBlock] 
      (_:_, _:_) -> appFun Prim.tryElseFinally [body, handler, elseBlock, finallyBlock]  
   where
   elseBlock = parens $ doBlock elseSuite
   finallyBlock = parens $ doBlock finally 

instance Compilable IdentSpan where
   type CompileResult IdentSpan = Hask.Exp
   compile = compile . toIdentString 

instance Compilable IdentString where
   type CompileResult IdentString = Hask.Exp
   compile ident = do
      let str = identString ident
          mangled = mangle str
          hashedVal = intE $ fromIntegral $ hash str
      return $ tuple [hashedVal, strE mangled] 

instance Compilable ExprSpan where
   type (CompileResult ExprSpan) = ([Stmt], Exp)

   compile (Py.Strings { strings_strings = ss }) = 
      returnExp $ Prim.string $ concat $ map trimString ss 
   compile (Py.Bool { bool_value = b}) = returnExp $ Prim.bool b
   compile (Py.Int { int_value = i}) = returnExp $ intE i
   compile (Py.Var { var_ident = ident}) = do
      global <- isGlobalIdent ident
      if global 
         then do
            compiledIdent <- compile ident
            returnExp $ app Prim.global compiledIdent
         else 
            returnExp $ app Prim.read $ identToMangledVar ident
   compile (Py.BinaryOp { operator = op, left_op_arg = leftExp, right_op_arg = rightExp }) 
      | Dot {} <- op, Py.Var { var_ident = method } <- rightExp = do
           (leftStmts, compiledLeft) <- compileExprObject leftExp
           compiledMethod <- compile method
           let newExp = infixApp compiledLeft (Prim.opExp op) compiledMethod 
           return (leftStmts, newExp)
      | otherwise = do
           (leftStmts, compiledLeft) <- compileExprObject leftExp
           (rightStmts, compiledRight) <- compileExprObject rightExp
           let newExp = infixApp compiledLeft (Prim.opExp op) compiledRight
           return (leftStmts ++ rightStmts, newExp)
   compile (Call { call_fun = fun, call_args = args }) = do
      (funStmts, compiledFun) <- compileExprObject fun
      (argsStmtss, compiledArgs) <- mapAndUnzipM compile args 
      let newExp = infixApp compiledFun Prim.apply (listE compiledArgs)
      return (funStmts ++ concat argsStmtss, newExp) 
   compile (Py.Tuple { tuple_exprs = elements }) = do
      (stmtss, exprs) <- mapAndUnzipM compileExprObject elements 
      let newExp = app Prim.tuple $ listE exprs
      return (concat stmtss, newExp)
   compile (Py.Lambda { lambda_args = params, lambda_body = body }) = do
      bindings <- checkEither $ funBindings params body
      compiledBody <- nestedScope bindings $ compileExprBlock body
      let args = Hask.PList $ map (identToMangledPatVar . paramIdent) params
      let lambda = lamE bogusSrcLoc [args] compiledBody
      returnExp $ app Prim.lambda $ parens lambda
   compile (Py.List { list_exprs = elements }) = do
      (stmtss, exprs) <- mapAndUnzipM compileExprObject elements 
      let newExp = app Prim.list $ listE exprs
      return (concat stmtss, newExp)

   compile (Py.Subscript { subscriptee = obj_expr, subscript_expr = sub }) = do
      (stmtss, exprs) <- mapAndUnzipM compileExprObject [obj_expr, sub]
      let newExp = appFun Prim.subscript exprs
      return (concat stmtss, newExp)

   compile (Py.Paren { paren_expr = e }) = compile e
   compile other = unsupported $ "compile: " ++ show other

instance Compilable ArgumentSpan where
   type (CompileResult ArgumentSpan) = ([Stmt], Exp)
   compile (ArgExpr { arg_expr = expr }) = compileExprObject expr

newtype Block = Block [StatementSpan]

instance Compilable Block where
   type (CompileResult Block) = [Hask.Stmt]
   compile (Block stmts) = do
      scope <- getScope 
      let locals = localVars scope
      varDecls <- mapM declareVar $ Set.toList locals
      haskStmtss <- compile stmts
      return (varDecls ++ concat haskStmtss)

-- This compiles an Expression to something with type (Eval Object). In cases where
-- the expression is atomic, it wraps the result in a call to "pure".
-- This is because compiling an atomic expression gives something
-- of type Object.
compileExprComp :: Py.ExprSpan -> Compile ([Stmt], Exp)
compileExprComp exp 
   | isAtomicExpr exp = do
        (stmts, compiledExp) <- compile exp
        return (stmts, app Prim.pure $ parens compiledExp)
   | otherwise = compile exp

-- This compiles an expression to something with type Object. In cases where
-- the expression is non-atomic, it binds the result of evaluating the expression
-- to a variable. This is because compiling a non-atomic expression gives something
-- of type (Eval Object)
compileExprObject :: Py.ExprSpan -> Compile ([Stmt], Exp)
compileExprObject exp
   | isAtomicExpr exp = compile exp
   | otherwise = do
      (expStmts, compiledExp) <- compile exp
      (binderStmts, binderExp) <- stmtBinder compiledExp 
      return (expStmts ++ binderStmts, binderExp)

compileHandlers :: Exp -> [HandlerSpan] -> Compile Exp 
compileHandlers asName handlers = do
   validate handlers 
   foldrM (compileHandler asName) Prim.pass handlers 

compileHandler :: Exp -> HandlerSpan -> Exp -> Compile Exp
compileHandler asName handler@(Handler { handler_clause = clause, handler_suite = body }) nextHandler = do
   bodyStmts <- compile body
   case except_clause clause of
      Nothing -> return $ appFun Prim.exceptDefault
                    [parens $ doBlock $ concat bodyStmts, parens nextHandler]
      Just (exceptClass, maybeExceptVar) -> do
         varStmts <- 
            case maybeExceptVar of
               Nothing -> return [] 
               Just (Py.Var { var_ident = ident }) -> do
                  identDecl <- declareVar ident
                  let newAssign = qualStmt $ infixApp (var $ identToMangledName ident) Prim.assignOp asName 
                  return [identDecl, newAssign]
         (classStmts, classObj) <- compileExprObject exceptClass
         let newBody = parens $ doBlock (varStmts ++ concat bodyStmts)
             newStmt = qualStmt $ appFun Prim.except [asName, classObj, newBody, parens nextHandler]
         return $ doBlock (classStmts ++ [newStmt]) 


compileAssign :: Py.ExprSpan -> Py.ExprSpan -> Compile [Stmt] 
-- Right argument of dot is always a variable, because dot associates to the left
compileAssign (Py.BinaryOp { operator = Dot {}
                           , left_op_arg = lhs 
                           , right_op_arg = Py.Var { var_ident = attribute}} 
              ) rhs = do
   (stmtsLhs, compiledLhs) <- compileExprObject lhs 
   (stmtsRhs, compiledRhs) <- compileExprObject rhs 
   compiledAttribute <- compile attribute
   let newStmt = qualStmt $ appFun Prim.setAttr [compiledLhs, compiledAttribute, compiledRhs]
   return (stmtsLhs ++ stmtsRhs ++ [newStmt])
compileAssign (Py.Var { var_ident = ident}) expr = do
   global <- isGlobalIdent ident 
   (exprStmts, compiledExp) <- compileExprObject expr
   if global 
      then do
         compiledIdent <- compile ident
         (binderStmts, binderExp) <- stmtBinder $ app Prim.globalRef compiledIdent 
         let newStmt = qualStmt $ infixApp binderExp Prim.assignOp compiledExp
         return (exprStmts ++ binderStmts ++ [newStmt])
      else do
         let newStmt = qualStmt $ infixApp (identToMangledVar ident) Prim.assignOp compiledExp
         return (exprStmts ++ [newStmt])
{-
Compile for loops by desugaring into while loops.

   for vars in exp:
      suite1
   else:
      suite2

desugars to --->

   fresh_var_1 = exp.__iter__()
   fresh_var_2 = True
   while fresh_var_2:
      try:
         vars = fresh_var_1.__next__()
         suite1
      except StopIteration:
         fresh_var_2 = False
   else:
      suite2

Note: the fresh_var_2 variable is used to break the loop instead of using a break statement
because of the way that Python treats "else" blocks in while loops. The else block
is only executed if the while loop terminates normally (not via break). (Yes it is
weird). This transformation should handle break and continue correctly when they appear
inside suite1
-}

desugarFor :: StatementSpan -> Compile [StatementSpan]
desugarFor (For { for_targets = targets, for_generator = generator, for_body = suite1, for_else = suite2}) = do

   freshVar1 <- freshPythonVar
   freshVar2 <- freshPythonVar

   let annot = annot
       freshVar1Exp = Py.Var { var_ident = freshVar1, expr_annot = annot }
       dotOperator = Dot { op_annot = annot }
       iter = Py.Var { var_ident = Py.Ident { ident_string = "__iter__", ident_annot = annot }, expr_annot = annot }
       expDotIter = BinaryOp { operator = dotOperator, left_op_arg = generator,  
                               right_op_arg = iter, expr_annot = annot } 
       expDotIterCall = Call { call_fun = expDotIter, call_args = [], expr_annot = annot }
       freshVar1Assign = Assign { assign_to = [freshVar1Exp], assign_expr = expDotIterCall, stmt_annot = annot }
   
       freshVar2Exp = Py.Var { var_ident = freshVar2, expr_annot = annot }
       true = Py.Var { var_ident = Py.Ident { ident_string = "True", ident_annot = annot }, expr_annot = annot }
       false = Py.Var { var_ident = Py.Ident { ident_string = "False", ident_annot = annot }, expr_annot = annot }
       freshVar2AssignTrue = Assign { assign_to = [freshVar2Exp], assign_expr = true, stmt_annot = annot }

       next = Py.Var { var_ident = Py.Ident { ident_string = "__next__", ident_annot = annot }, expr_annot = annot }
       freshVar1DotNext = BinaryOp { operator = dotOperator, left_op_arg = freshVar1Exp,  
                                     right_op_arg = next, expr_annot = annot }
       freshVar1DotNextCall = Call { call_fun = freshVar1DotNext, call_args = [], expr_annot = annot }
       freshVar2AssignFalse = Assign { assign_to = [freshVar2Exp], assign_expr = false, stmt_annot = annot }
       stopIteration = Py.Var { var_ident = Py.Ident { ident_string = "StopIteration", ident_annot = annot }, expr_annot = annot }
       targetsAssign = Assign { assign_to = targets, assign_expr = freshVar1DotNextCall, stmt_annot = annot }
       stopIterationClause = ExceptClause { except_clause = Just (stopIteration, Nothing), except_clause_annot = annot }
       stopIterationHandler = Handler { handler_clause = stopIterationClause, handler_suite = [freshVar2AssignFalse], handler_annot = annot } 

       tryBlock = Try { try_body = targetsAssign:suite1, try_excepts = [stopIterationHandler], try_else = [], try_finally = [], stmt_annot = annot }

       whileLoop = While { while_cond = freshVar2Exp, while_body = [tryBlock], while_else = suite2, stmt_annot = annot } 
   return [freshVar1Assign, freshVar2AssignTrue, whileLoop]

stmtBinder :: Exp -> Compile ([Stmt], Exp)
stmtBinder exp = do
   v <- freshHaskellVar
   let newStmt = genStmt bogusSrcLoc (pvar v) exp
   return ([newStmt], var v)

compileExprBlock :: ExprSpan -> Compile Hask.Exp
compileExprBlock exp = do
    (stmts, exp) <- compileExprComp exp
    return $ doBlock (stmts ++ [qualStmt exp])

compileBlockDo :: Block -> Compile Hask.Exp
compileBlockDo block = doBlock <$> compile block 

compileSuiteDo :: SuiteSpan -> Compile Exp
compileSuiteDo stmts = do
   compiledStmtss <- compile stmts
   return $ doBlock $ concat compiledStmtss 

nestedScope :: Scope -> Compile a -> Compile a
nestedScope bindings comp = do
   outerScope <- getScope
   let newEnclosingVars = enclosingVars outerScope `Set.union` 
                          localVars outerScope `Set.union`
                          paramVars outerScope
   let newLevel = nestingLevel outerScope + 1
       newScope = bindings { nestingLevel = newLevel, enclosingVars = newEnclosingVars }
   local (const newScope) comp

returnStmt :: Exp -> Compile [Stmt]
returnStmt e = return [qualStmt e]

returnExp :: Exp -> Compile ([Stmt], Exp)
returnExp e = return ([], e)

declareVar :: ToIdentString a => a -> Compile Hask.Stmt
declareVar ident = do
   topLevel <- isTopLevel 
   let declaration = if topLevel then Prim.globalVariable else Prim.variable
       mangledPatVar = identToMangledPatVar ident
   compiledIdent <- compile $ toIdentString ident
   return $ genStmt bogusSrcLoc mangledPatVar (app declaration compiledIdent)

compileGuard :: Hask.Exp -> (ExprSpan, SuiteSpan) -> Compile Hask.Exp
compileGuard elseExp (guard, body) = 
   conditional <$> compileExprBlock guard <*> compileSuiteDo body <*> pure elseExp

isGlobalIdent :: Py.IdentSpan -> Compile Bool
isGlobalIdent ident = do
   scope <- getScope
   topLevel <- isTopLevel 
   let locals = localVars scope
       enclosings = enclosingVars scope
       globals = globalVars scope
       params = paramVars scope
       identStr = toIdentString ident
   return $ 
      if topLevel 
         -- XXX could be an error if the variable is not defined yet 
         then identStr `Set.notMember` locals
         -- XXX this might not strictly be necessary if the ident is not in the enclosings
         else identStr `Set.member` globals ||
              all (identStr `Set.notMember`) [locals, enclosings, params]

imports :: [ImportDecl]
imports = [importBerp, importPrelude]

importBerp :: ImportDecl
importBerp = 
   ImportDecl
   { importLoc = bogusSrcLoc 
   , importModule = Prim.berpModuleName 
   , importQualified = False 
   , importSrc = False 
   , importAs  = Nothing 
   , importSpecs = Nothing 
   , importPkg = Nothing
   }

importPrelude :: ImportDecl
importPrelude = 
   ImportDecl
   { importLoc = bogusSrcLoc 
   , importModule = Prim.preludeModuleName 
   , importQualified = True 
   , importSrc = False 
   , importAs  = Nothing 
   , importSpecs = Nothing 
   , importPkg = Nothing
   }

pyIdentToHaskName :: Py.IdentSpan -> Hask.Name 
pyIdentToHaskName (Py.Ident { ident_string = s}) = name s 

identToMangledName :: ToIdentString a => a -> Hask.Name
identToMangledName = name . mangle . identString  

identToMangledVar :: ToIdentString a => a -> Hask.Exp
identToMangledVar = var . identToMangledName

identToMangledPatVar :: ToIdentString a => a -> Hask.Pat
identToMangledPatVar = pvar . identToMangledName

-- Check that the syntax is valid Python (the parser is sometimes too liberal).
class Validate t where
   validate :: t -> Compile ()

instance Validate [HandlerSpan] where
   validate [] = fail "Syntax Error: Syntax Error: try statement must have one or more handlers"
   validate [h] = return ()
   validate (h:hs) 
       | Nothing <- except_clause $ handler_clause h
            = if null hs then return () 
                         else fail "Syntax Error: default 'except:' must be last"
       | otherwise = validate hs

-- Trim (one or three) quote marks off front and end of string which are left by the lexer/parser.
trimString :: String -> String
trimString [] = []
trimString (w:x:y:zs)
   | all isQuote [w,x,y] && all (== w) [x,y] = trimStringEnd zs
   | isQuote w = trimStringEnd (x:y:zs)
   | otherwise = w:x:y:trimStringEnd zs
trimString (x:xs)
   | isQuote x = trimStringEnd xs
   | otherwise = x : trimStringEnd xs

trimStringEnd [] = [] 
trimStringEnd str@[x]
      | isQuote x = []
      | otherwise = str
trimStringEnd str@[x,y,z]
      | all isQuote str && all (== x) [y,z] = []
      | otherwise = x : trimStringEnd [y,z] 
trimStringEnd (x:xs) = x : trimStringEnd xs 

isQuote '\'' = True
isQuote '"' = True
isQuote _ = False 
