module Berp.Compile.HsSyntaxUtils  where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Build
import Berp.Compile.PrimName as Prim

bogusSrcLoc = SrcLoc { srcFilename = "", srcLine = -1, srcColumn = -1 }

class Parens a where
   parens :: a -> a

-- not exhaustive
instance Parens Exp where
   parens e@(Var {}) = e
   parens e@(IPVar {}) = e
   parens e@(Con {}) = e
   parens e@(Lit {}) = e
   parens e@(Tuple {}) = e
   parens e@(List {}) = e
   parens e@(Paren {}) = e
   parens e@(ListComp {}) = e
   parens e = paren e

-- turn a list of statements into a do block, avoiding redundant do.
doBlock :: [Stmt] -> Exp
doBlock [Qualifier e] = e
doBlock stmts = doE stmts

conditional :: Exp -> Exp -> Exp -> Exp
conditional cond trueBranch falseBranch
   = appFun Prim.ite [parens cond, parens trueBranch, parens falseBranch]

