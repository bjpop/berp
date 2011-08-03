{-# LANGUAGE PatternGuards, TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.Compile
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The compiler for berp. The compiler translates Python 3 into Haskell.
--
-----------------------------------------------------------------------------

module Berp.Compile.Compile (compiler, Compilable (..)) where

import Prelude hiding (read, init, mapM, putStrLn, sequence)
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.AST as Py
import Data.Traversable
import Data.Foldable (foldrM)
import Language.Haskell.Exts.Syntax as Hask
import Language.Haskell.Exts.Build as Hask
import Control.Applicative
import qualified Data.Set as Set
import Data.Set ((\\))
import Control.Monad hiding (mapM, sequence)
import qualified Berp.Compile.PrimName as Prim
import Berp.Compile.Monad
import Berp.Compile.HsSyntaxUtils as Hask
import Berp.Compile.PySyntaxUtils as Py
import Berp.Compile.Utils
import Berp.Base.Mangle (mangle)
import Berp.Base.Hash (Hash (..))
import Berp.Compile.IdentString (IdentString (..), ToIdentString (..), identString)
import Berp.Compile.Scope as Scope (Scope (..), topBindings, funBindings)
import qualified Berp.Compile.Scope as Scope (isGlobal)

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

instance Compilable InterpreterStmt where
   type CompileResult InterpreterStmt = Hask.Exp
   compile (InterpreterStmt suite) = do
      stmts <- nestedScope topBindings $ compile $ Block suite
      return $ mkLambda stmts
      where
      mkLambda :: [Hask.Stmt] -> Hask.Exp
      mkLambda stmts = lamE bogusSrcLoc [Prim.globalsPat] $ doBlock stmts

instance Compilable ModuleSpan where
   type CompileResult ModuleSpan = String -> (Hask.Module, [String])
   compile (Py.Module suite) = do
      stmts <- nestedScope topBindings $ compile $ Block suite
      -- let allStmts = stmts ++ [qualStmt Prim.mkModule]
      importedModules <- Set.toList <$> getImports
      return $ \modName ->
                   let init = initDecl $ doBlock stmts in
                   (Hask.Module bogusSrcLoc
                      (ModuleName modName) pragmas warnings exports
                      (imports importedModules) [init],
                    importedModules)
      where
      initDecl :: Hask.Exp -> Hask.Decl
      -- initDecl = patBind bogusSrcLoc $ pvar $ name initName
      initDecl = simpleFun bogusSrcLoc (name initName) (name Prim.globalsName)
      pragmas = []
      warnings = Nothing
      exports = Just [EVar $ UnQual $ name initName]
      srcImports names = mkImportStmts $ map mkSrcImport names
      stdImports = mkImportStmts [(Prim.preludeModuleName,False,Just []),
                                  (Prim.berpModuleName,False,Nothing)]
      imports names = stdImports ++ srcImports (map mkBerpModuleName names)

initName :: String
initName = "init"

mkSrcImport :: String -> (ModuleName, Bool, Maybe [String])
mkSrcImport name = (ModuleName name, True, Just [initName])

mkImportStmts :: [(ModuleName, Bool, Maybe [String])] -> [ImportDecl]
mkImportStmts = map toImportStmt

toImportStmt :: (ModuleName, Bool, Maybe [String]) -> ImportDecl
toImportStmt (moduleName, qualified, items) =
   ImportDecl
   { importLoc = bogusSrcLoc
   , importModule = moduleName
   , importQualified = qualified
   , importSrc = False
   , importAs  = Nothing
   , importSpecs = mkImportSpecs items
   , importPkg = Nothing
   }

mkImportSpecs :: Maybe [String] -> Maybe (Bool, [ImportSpec])
mkImportSpecs Nothing = Nothing
mkImportSpecs (Just items) = Just (False, map (IVar . name) items)

instance Compilable StatementSpan where
   type (CompileResult StatementSpan) = [Stmt]

   -- XXX is it necessary to compile generators this way?
   -- can the yield statement be made to dynamically build a generator object?
   compile (Fun {fun_name = fun, fun_args = params, fun_body = body}) = do
      oldSeenYield <- getSeenYield
      unSetSeenYield
      bindings <- checkEither $ funBindings params body
      compiledBody <- nestedScope bindings $ compileBlockDo $ Block body
      let args = Hask.PList $ map (identToMangledPatVar . paramIdent) params
      isGenerator <- getSeenYield
      setSeenYield oldSeenYield
      let lambdaBody =
             if isGenerator
                -- this warrants a special case of returnGenerator, otherwise
                -- the compiled code is slightly uglier.
                then app Prim.returnGenerator $ parens compiledBody
                else compiledBody
          lambda = lamE bogusSrcLoc [args] lambdaBody
          arityExp = intE $ fromIntegral $ length params
          doc = docString body
          defExp = appFun Prim.def [arityExp, doc, parens lambda]
      (binderStmts, binderExp) <- stmtBinder defExp
      writeStmt <- qualStmt <$> compileWrite fun binderExp
      return $ binderStmts ++ [writeStmt]

   -- Handle the single assignment case specially. In the multi-assign case
   -- we want to share the evaluation of the rhs, hence the use of compileExprComp.
   -- This is not needed in the single assign case, and would be overkill in general.
   compile (Assign { assign_to = [lhs], assign_expr = rhs }) = do
      (stmtsRhs, compiledRhs) <- compileExprObject rhs
      assignStmts <- compileAssign lhs compiledRhs
      return $ stmtsRhs ++ assignStmts
   compile (Assign { assign_to = lhss, assign_expr = rhs }) = do
      (stmtsRhs, compiledRhs) <- compileExprComp rhs
      (binderStmts, binderExp) <- stmtBinder compiledRhs
      assignStmtss <- mapM (flip compileAssign binderExp) lhss
      return $ stmtsRhs ++ binderStmts ++ concat assignStmtss
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
   compile (Return { return_expr = maybeExpr })
      | Just call@(Call {}) <- maybeExpr = do
           (stmts, compiledExpr) <- compileTailCall call
           let newStmt = qualStmt compiledExpr
           return (stmts ++ [newStmt])
      | otherwise = do
           (stmts, compiledExpr) <- maybe (returnExp Prim.none) compileExprObject maybeExpr
           let newStmt = qualStmt $ app Prim.ret $ parens compiledExpr
           return (stmts ++ [newStmt])
   {-
      Even though it looks like we could eliminate stmt expressions, we do need to 
      compile them to code just in case they have side effects (like raising exceptions).
      It is very hard to determine that an expression is effect free. Constant values
      are the easy case, but probably not worth the effort. Furthermore, top-level
      constant expressions must be preserved for the repl of the interpreter.
   -}
   compile (StmtExpr { stmt_expr = expr }) = do
      (stmts, compiledExpr) <- compileExprComp expr
      let newStmt = qualStmt $ compiledExpr
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
   compile (For { for_targets = [var], for_generator = generator, for_body = body, for_else = elseSuite }) = do
      (generatorStmts, compiledGenerator) <- compileExprObject generator
      compiledStmtss <- compile body
      newVar  <- freshHaskellVar
      writeStmt <- qualStmt <$> compileWrite var (Hask.var newVar)
      let lambdaBody = lamE bogusSrcLoc [pvar newVar] $ doBlock $ (writeStmt : concat compiledStmtss)
      if isEmptySuite elseSuite
         then return (generatorStmts ++ [qualStmt $ appFun Prim.for [compiledGenerator, parens lambdaBody]])
         else do
            compiledElse <- compileSuiteDo elseSuite
            return (generatorStmts ++ [qualStmt $ appFun Prim.forElse [compiledGenerator, parens lambdaBody, parens compiledElse]])
   compile (Pass {}) = returnStmt Prim.pass
   compile (NonLocal {}) = return []
   compile (Global {}) = return []
   -- XXX need to check if we are compiling a local or global variable.
   -- XXX perhaps the body of a class is evaluated in a similar fashion to a module?
   -- more dynamic than currently. Yes it is!
   compile (Class { class_name = ident, class_args = args, class_body = body }) = do
      bindings <- checkEither $ funBindings [] body
      -- XXX slightly dodgy since the syntax allows Argument types in class definitions but
      -- I'm not sure what their meaning is, or if it is just a case of the grammar over specifying
      -- the language
      (argsStmtss, compiledArgs) <- mapAndUnzipM (compileExprObject . arg_expr) args
      compiledBody <- nestedScope bindings $ compile $ Block body
      let locals = Set.toList $ localVars bindings
      attributes <- qualStmt <$> app Prim.pure <$> listE <$> mapM compileClassLocal locals
      let klassExp = appFun Prim.klass
                        [ strE $ identString ident
                        , listE compiledArgs
                        , parens $ doBlock $ compiledBody ++ [attributes]]

      (binderStmts, binderExp) <- stmtBinder klassExp
      writeStmt <- qualStmt <$> compileWrite ident binderExp

      return (concat argsStmtss ++ binderStmts ++ [writeStmt])
      where
      compileClassLocal :: IdentString -> Compile Hask.Exp
      compileClassLocal ident = do
         hashedIdent <- compile ident
         let mangledIdent = identToMangledVar ident
         return $ Hask.tuple [hashedIdent, mangledIdent]
   compile (Try { try_body = body, try_excepts = handlers, try_else = elseSuite, try_finally = finally }) = do
      bodyExp <- compileSuiteDo body
      asName <- freshHaskellVar
      handlerExp <- compileHandlers (Hask.var asName) handlers
      let handlerLam = lamE bogusSrcLoc [pvar asName] handlerExp
      compiledElse <- compile elseSuite
      compiledFinally <- compile finally
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
   compile (Import { import_items = items }) = concat <$> mapM compile items
   compile stmt@(FromImport { from_module = mod, from_items = items }) = do
      case import_relative_module mod of
         Nothing -> unsupported $ prettyText stmt
         Just dottedName ->
            case dottedName of
               [ident] -> do
                  let identStr = ident_string ident
                      berpIdentStr = mkBerpModuleName identStr
                      importExp = appFun Prim.importModule
                                     [strE identStr, qvar (ModuleName berpIdentStr) (name initName)]
                  (binderStmts, binderExp) <- stmtBinder importExp
                  itemsStmts <- compileFromItems binderExp items
                  addImport identStr
                  return (binderStmts ++ itemsStmts)
               _other -> unsupported $ prettyText stmt

   compile other = unsupported $ prettyText other

compileRead :: ToIdentString a => a -> Compile Exp
compileRead ident = do
   compiledIdent <- compile $ toIdentString ident
   global <- isGlobal ident
{-
   let reader = if global then Prim.readGlobal else Prim.readLocal
   return $ appFun reader [Prim.globals, compiledIdent]
-}
   return $
      if global
         then appFun Prim.readGlobal [Prim.globals, compiledIdent]
         else appFun Prim.readLocal [compiledIdent]

compileWrite :: ToIdentString a => a -> Exp -> Compile Exp
compileWrite ident exp = do
   compiledIdent <- compile $ toIdentString ident
   global <- isGlobal ident
{-
   let writer = if global then Prim.writeGlobal else Prim.writeLocal
   return $ appFun writer [Prim.globals, compiledIdent, exp]
-}
   return $
      if global
         then appFun Prim.writeGlobal [Prim.globals, compiledIdent, exp]
         else appFun Prim.writeLocal [compiledIdent, exp]

compileFromItems :: Exp -> FromItemsSpan -> Compile [Hask.Stmt]
compileFromItems exp (ImportEverything {}) = do
   topLevel <- isTopLevel
   if topLevel
      then returnStmt $ appFun Prim.importAll [Prim.globals, exp]
      else fail "Syntax Error: import * only allowed at module level"
compileFromItems exp (FromItems { from_items_items = items }) =
   concat <$> mapM (compileFromItem exp) items

compileFromItem :: Exp -> FromItemSpan -> Compile [Hask.Stmt]
compileFromItem exp (FromItem { from_item_name = item, from_as_name = maybeAsName }) = do
   let objectName = maybe item id maybeAsName
   compiledItem <- compile item
   (projectStmts, obj)  <- stmtBinder (infixApp exp (Prim.primOp ".") compiledItem)
   writeStmt <- qualStmt <$> compileWrite objectName obj
   return (projectStmts ++ [writeStmt])

instance Compilable ImportItemSpan where
   type CompileResult ImportItemSpan = [Hask.Stmt]
   compile (ImportItem {import_item_name = dottedName, import_as_name = maybeAsName }) =
      case dottedName of
         [ident] -> do
            let identStr = ident_string ident
                berpIdentStr = mkBerpModuleName identStr
                importExp = appFun Prim.importModule
                                     [strE identStr, qvar (ModuleName berpIdentStr) (name initName)]
            (binderStmts, binderExp) <- stmtBinder importExp
            let objectName = maybe ident id maybeAsName
            writeStmt <- qualStmt <$> compileWrite objectName binderExp
            addImport identStr
            return (binderStmts ++ [writeStmt])
         _other -> unsupported ("import of " ++ show dottedName)

mkBerpModuleName :: String -> String
mkBerpModuleName = ("Berp_" ++)

docString :: SuiteSpan -> Exp
docString (StmtExpr { stmt_expr = Strings { strings_strings = ss }} : _)
   = parens $ Prim.string $ trimString $ concat ss
docString _other = Prim.none

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
       global <- isGlobal ident
       if global
          then do
             let str = identString $ toIdentString ident
                 mangled = mangle str
                 -- hashedVal = intE $ fromIntegral $ hash str
                 hashedVal = intE $ fromIntegral $ hash mangled
             return $ Hask.tuple [hashedVal, strE mangled]
          else
             return $ identToMangledVar ident

instance Compilable ExprSpan where
   type (CompileResult ExprSpan) = ([Stmt], Exp)

   compile (Py.Strings { strings_strings = ss }) =
      returnExp $ Prim.string $ concat $ map trimString ss
   compile (Py.Bool { bool_value = b}) = returnExp $ Prim.bool b
   compile (Py.Int { int_value = i}) = returnExp $ intE i
   compile (Py.Float { float_value = f}) = returnExp $ Lit $ Frac $ toRational f
   compile (Py.Imaginary { imaginary_value = i}) =
      returnExp $ app Prim.complex $ paren c
      where
      real = Lit $ Frac 0
      imag = Lit $ Frac $ toRational i
      c = infixApp real (op $ sym ":+") imag
   compile (Py.Var { var_ident = ident}) = do
      readExp <- compileRead ident
      return ([], readExp)
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
   compile (Py.UnaryOp { operator = op, op_arg = arg }) = do
      (argStmts, compiledArg) <- compileExprObject arg
      let compiledOp = compileUnaryOp op
      return (argStmts, app compiledOp compiledArg)
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
      returnExp $ appFun Prim.lambda [intE (fromIntegral $ length params), parens lambda]
   compile (Py.List { list_exprs = elements }) = do
      (stmtss, exprs) <- mapAndUnzipM compileExprObject elements
      let newExp = app Prim.list $ listE exprs
      return (concat stmtss, newExp)
   compile (Py.Dictionary { dict_mappings = mappings }) = do
      let compileExprObjectPair (e1, e2) = do
             (stmts1, compiledE1) <- compileExprObject e1
             (stmts2, compiledE2) <- compileExprObject e2
             return (stmts1 ++ stmts2, (compiledE1, compiledE2))
      (stmtss, exprPairs) <- mapAndUnzipM compileExprObjectPair mappings
      let newExp = app Prim.dict $ listE $ map (\(x,y) -> Hask.tuple [x,y]) exprPairs
      return (concat stmtss, newExp)
   compile (Py.Set { set_exprs = elements }) = do
      (stmtss, exprs) <- mapAndUnzipM compileExprObject elements
      let newExp = app Prim.set $ listE exprs
      return (concat stmtss, newExp)
   compile (Subscript { subscriptee = obj_expr, subscript_expr = sub }) = do
      (stmtss, exprs) <- mapAndUnzipM compileExprObject [obj_expr, sub]
      let newExp = appFun Prim.subscript exprs
      return (concat stmtss, newExp)
   compile (Yield { yield_expr = maybeExpr }) = do
      (stmts, compiledExpr) <- maybe (returnExp Prim.none) compileExprObject maybeExpr
      let newExpr = app Prim.yield $ parens compiledExpr
      setSeenYield True
      return (stmts, newExpr)
   compile (Py.Paren { paren_expr = e }) = compile e
   compile (None {}) = returnExp Prim.none
   compile (Py.Generator { gen_comprehension = comp }) = compileComprehens GenComprehension comp
   compile (Py.ListComp { list_comprehension = comp }) = compileComprehens ListComprehension comp
   compile (DictComp { dict_comprehension = comp }) =
      compileComprehens DictComprehension $ normaliseDictComprehension comp
   compile (SetComp { set_comprehension = comp }) = compileComprehens SetComprehension comp
   compile other = unsupported $ prettyText other

data ComprehensType = GenComprehension | ListComprehension | DictComprehension | SetComprehension
   deriving (Eq, Show)

-- XXX maybe it would make more sense if we normalised dict comprehensions in the parser.
-- could simplify the types somewhat.
normaliseDictComprehension :: ComprehensionSpan (ExprSpan, ExprSpan) -> ComprehensionSpan ExprSpan
normaliseDictComprehension comp@(Comprehension { comprehension_expr = (e1, e2) })
   = comp { comprehension_expr = Py.tuple [e1, e2] }

compileComprehens :: ComprehensType -> ComprehensionSpan ExprSpan -> Compile ([Stmt], Exp)
compileComprehens GenComprehension comprehension = do
   let resultStmt = stmtExpr $ yield $ comprehension_expr comprehension
   desugaredFor <- desugarComprehensFor resultStmt $ comprehension_for comprehension
   bindings <- checkEither $ funBindings [] desugaredFor
   compiledBody <- nestedScope bindings $ compileBlockDo $ Block [desugaredFor]
   let mkGenApp = app Prim.generator $ parens compiledBody
   return ([], mkGenApp)

compileComprehens ty comprehension = do
   v <- freshPythonVar
   let newVar = Py.var v
   let initStmt = comprehensInit ty newVar
       resultStmt = comprehensUpdater ty newVar $ comprehension_expr comprehension
   desugaredFor <- desugarComprehensFor resultStmt $ comprehension_for comprehension
   bindings <- checkEither $ funBindings [] desugaredFor
   let oldLocals = localVars bindings
       newLocals = Set.insert (toIdentString v) oldLocals
   let newBindings = bindings { localVars = newLocals }
   compiledBody <- nestedScope newBindings $ compile $ Block [initStmt, desugaredFor]
   -- XXX this should be a readLocal
   return (compiledBody, app Prim.read $ identToMangledVar v)

comprehensInit :: ComprehensType -> ExprSpan -> StatementSpan
comprehensInit ListComprehension var = var `assign` list []
comprehensInit SetComprehension var = var `assign` set []
comprehensInit DictComprehension var = var `assign` dict []
comprehensInit GenComprehension _var = error $ "comprehensInit called on generator comprehension"

comprehensUpdater :: ComprehensType -> ExprSpan -> ExprSpan -> StatementSpan
comprehensUpdater ListComprehension lhs rhs =
   stmtExpr $ call (binOp dot lhs $ Py.var $ ident "append") [rhs]
comprehensUpdater SetComprehension lhs rhs =
   stmtExpr $ call (binOp dot lhs $ Py.var $ ident "add") [rhs]
comprehensUpdater DictComprehension lhs (Py.Tuple { tuple_exprs = [key, val] }) =
   assign (subscript lhs key) val
comprehensUpdater GenComprehension _lhs _rhs =
   error $ "comprehensUpdater called on generator comprehension"
comprehensUpdater _other _lhs _rhs =
   error $ "comprehensUpdater called on badly formed comprehension"

desugarComprehensFor :: StatementSpan -> CompForSpan -> Compile StatementSpan
desugarComprehensFor result
      (CompFor { comp_for_exprs = pat, comp_in_expr = inExpr, comp_for_iter = rest }) = do
   stmts <- desugarComprehensMaybeIter result rest
   return $ Py.for pat inExpr stmts

desugarComprehensMaybeIter :: StatementSpan -> (Maybe CompIterSpan) -> Compile [StatementSpan]
desugarComprehensMaybeIter result Nothing = return [result]
desugarComprehensMaybeIter result (Just iter) = desugarComprehensIter result iter

desugarComprehensIter :: StatementSpan -> CompIterSpan -> Compile [StatementSpan]
desugarComprehensIter result (IterFor { comp_iter_for = compFor })
   = (:[]) <$> desugarComprehensFor result compFor
desugarComprehensIter result (IterIf { comp_iter_if = compIf })
   = desugarComprehensIf result compIf

desugarComprehensIf :: StatementSpan -> CompIfSpan -> Compile [StatementSpan]
desugarComprehensIf result (CompIf { comp_if = ifPart, comp_if_iter = iter }) = do
   stmts <- desugarComprehensMaybeIter result iter
   let guards = [(ifPart, stmts)]
   return [Py.conditional guards []]

compileTailCall :: ExprSpan -> Compile ([Stmt], Exp)
compileTailCall (Call { call_fun = fun, call_args = args }) = do
      (funStmts, compiledFun) <- compileExprObject fun
      (argsStmtss, compiledArgs) <- mapAndUnzipM compile args
      let newExp = appFun Prim.tailCall [compiledFun, listE compiledArgs]
      return (funStmts ++ concat argsStmtss, newExp)
compileTailCall other = error $ "compileTailCall on non call expression: " ++ show other

instance Compilable ArgumentSpan where
   type (CompileResult ArgumentSpan) = ([Stmt], Exp)
   compile (ArgExpr { arg_expr = expr }) = compileExprObject expr
   compile other = unsupported $ prettyText other

newtype Block = Block [StatementSpan]
newtype TopBlock = TopBlock [StatementSpan]

instance Compilable TopBlock where
   type (CompileResult TopBlock) = ([Hask.Stmt], [Hask.Stmt])
   compile (TopBlock []) = return ([], [qualStmt Prim.pass])
   compile (TopBlock stmts) = do
      scope <- getScope
      let locals = localVars scope
      varDecls <- mapM declareTopInterpreterVar $ Set.toList locals
      haskStmtss <- compile stmts
      return (varDecls, concat haskStmtss)

instance Compilable Block where
   type (CompileResult Block) = [Hask.Stmt]
   compile (Block []) = return [qualStmt Prim.pass]
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
        return (stmts, app Prim.pureObj $ parens compiledExp)
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
   foldrM (compileHandler asName) (parens $ app Prim.raise asName) handlers

compileHandler :: Exp -> HandlerSpan -> Exp -> Compile Exp
compileHandler asName (Handler { handler_clause = clause, handler_suite = body }) nextHandler = do
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
                  -- XXX I think this should always be a local variable assignment
                  let newAssign = qualStmt $ infixApp (Hask.var $ identToMangledName ident) Prim.assignOp asName
                  return [identDecl, newAssign]
               other -> error $ "exception expression not a variable: " ++ show other
         (classStmts, classObj) <- compileExprObject exceptClass
         let newBody = parens $ doBlock (varStmts ++ concat bodyStmts)
             newStmt = qualStmt $ appFun Prim.except [asName, classObj, newBody, parens nextHandler]
         return $ doBlock (classStmts ++ [newStmt])

compileAssign :: Py.ExprSpan -> Hask.Exp -> Compile [Stmt]
compileAssign (Py.Paren { paren_expr = expr }) rhs = compileAssign expr rhs
compileAssign (Py.Tuple { tuple_exprs = patElements }) rhs =
   compileUnpack patElements rhs
compileAssign (Py.List { list_exprs = patElements }) rhs =
   compileUnpack patElements rhs
-- Right argument of dot is always a variable, because dot associates to the left
compileAssign (Py.BinaryOp { operator = Dot {}
                           , left_op_arg = lhs
                           , right_op_arg = Py.Var { var_ident = attribute}}
              ) rhs = do
   (stmtsLhs, compiledLhs) <- compileExprObject lhs
   compiledAttribute <- compile attribute
   let newStmt = qualStmt $ appFun Prim.setAttr [compiledLhs, compiledAttribute, rhs]
   return (stmtsLhs ++ [newStmt])
compileAssign (Py.Subscript { subscriptee = objExpr, subscript_expr = sub }) rhs = do
   (stmtsObj, compiledObj) <- compileExprObject objExpr
   (stmtsSub, compiledSub) <- compileExprObject sub
   let newStmt = qualStmt $ appFun Prim.setItem [compiledObj, compiledSub, rhs]
   return (stmtsObj ++ stmtsSub ++ [newStmt])
compileAssign (Py.Var { var_ident = ident}) rhs =
   (:[]) <$> qualStmt <$> compileWrite ident rhs
compileAssign lhs _rhs = unsupported ("Assignment to " ++ prettyText lhs)

compileUnpack :: [Py.ExprSpan] -> Hask.Exp -> Compile [Stmt]
compileUnpack exps rhs = do
   let pat = mkUnpackPat exps
   returnStmt $ appFun Prim.unpack [pat, rhs]
   where
   mkUnpackPat :: [Py.ExprSpan] -> Hask.Exp
   mkUnpackPat listExps =
      appFun (Con $ UnQual $ name "G")
             [ intE $ fromIntegral $ length listExps
             , listE $ map unpackComponent listExps]
   unpackComponent :: Py.ExprSpan ->  Hask.Exp
   unpackComponent (Py.Var { var_ident = ident }) =
      App (Con $ UnQual $ name "V") (identToMangledVar ident)
   unpackComponent (Py.List { list_exprs = elements }) = mkUnpackPat elements
   unpackComponent (Py.Tuple { tuple_exprs = elements }) = mkUnpackPat elements
   unpackComponent (Py.Paren { paren_expr = exp }) = unpackComponent exp
   unpackComponent other = error $ "unpack assignment to " ++ prettyText other

compileUnaryOp :: Py.OpSpan -> Hask.Exp
compileUnaryOp (Plus {}) = Prim.unaryPlus
compileUnaryOp (Minus {}) = Prim.unaryMinus
compileUnaryOp (Invert {}) = Prim.invert
compileUnaryOp (Not {}) = Prim.not
compileUnaryOp other = error ("Syntax Error: not a valid unary operator: " ++ show other)

stmtBinder :: Exp -> Compile ([Stmt], Exp)
stmtBinder exp = do
   v <- freshHaskellVar
   let newStmt = genStmt bogusSrcLoc (pvar v) exp
   return ([newStmt], Hask.var v)

compileExprBlock :: ExprSpan -> Compile Hask.Exp
compileExprBlock exp = do
    (stmts, exp) <- compileExprComp exp
    return $ doBlock (stmts ++ [qualStmt exp])

compileBlockDo :: Block -> Compile Hask.Exp
compileBlockDo block = doBlock <$> compile block

compileSuiteDo :: SuiteSpan -> Compile Exp
compileSuiteDo [] = return Prim.pass
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
   setScope newScope
   result <- comp
   setScope outerScope
   return result

returnStmt :: Exp -> Compile [Stmt]
returnStmt e = return [qualStmt e]

returnExp :: Exp -> Compile ([Stmt], Exp)
returnExp e = return ([], e)

declareTopInterpreterVar :: ToIdentString a => a -> Compile Hask.Stmt
declareTopInterpreterVar ident = do
   let mangledPatVar = identToMangledPatVar ident
       str = strE $ identString ident
   return $ genStmt bogusSrcLoc mangledPatVar $ app Prim.topVar str

declareVar :: ToIdentString a => a -> Compile Hask.Stmt
declareVar ident = do
   let mangledPatVar = identToMangledPatVar ident
       str = strE $ identString ident
   return $ genStmt bogusSrcLoc mangledPatVar $ app Prim.variable str

compileGuard :: Hask.Exp -> (ExprSpan, SuiteSpan) -> Compile Hask.Exp
compileGuard elseExp (guard, body) =
   Hask.conditional <$> compileExprBlock guard <*> compileSuiteDo body <*> pure elseExp

identToMangledName :: ToIdentString a => a -> Hask.Name
identToMangledName = name . mangle . identString

identToMangledVar :: ToIdentString a => a -> Hask.Exp
identToMangledVar = Hask.var . identToMangledName

identToMangledPatVar :: ToIdentString a => a -> Hask.Pat
identToMangledPatVar = pvar . identToMangledName

-- Check that the syntax is valid Python (the parser is sometimes too liberal).
class Validate t where
   validate :: t -> Compile ()

instance Validate [HandlerSpan] where
   validate [] = fail "Syntax Error: try statement must have one or more handlers"
   validate [_] = return ()
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

trimStringEnd :: String -> String
trimStringEnd [] = []
trimStringEnd str@[x]
      | isQuote x = []
      | otherwise = str
trimStringEnd str@[x,y,z]
      | all isQuote str && all (== x) [y,z] = []
      | otherwise = x : trimStringEnd [y,z]
trimStringEnd (x:xs) = x : trimStringEnd xs

isQuote :: Char -> Bool
isQuote '\'' = True
isQuote '"' = True
isQuote _ = False 

-- test if a variable is global
isGlobal :: ToIdentString a => a -> Compile Bool
isGlobal ident = withScope $ \scope -> Scope.isGlobal (toIdentString ident) scope
