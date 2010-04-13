{-# LANGUAGE TypeSynonymInstances #-}
module Berp.Compile.IdentString where

import Language.Python.Common.AST

newtype IdentString = IdentString { fromIdentString :: String }
   deriving (Eq, Ord, Show)

class ToIdentString t where
   toIdentString :: t -> IdentString 

instance ToIdentString IdentString where
   toIdentString = id

instance ToIdentString String where
   toIdentString str = IdentString str

instance ToIdentString (Ident a) where
   toIdentString (Ident { ident_string = name }) = IdentString name

instance ToIdentString (Expr a) where
   toIdentString (Var { var_ident = ident }) = toIdentString ident
   toIdentString other = error "toIdentString applied to an expression which is not a variable"

identString :: ToIdentString a => a -> String
identString = fromIdentString . toIdentString
