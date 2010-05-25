{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Compile.IdentString
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Strings encoding identifiers and ways to convert other things to them.
--
-----------------------------------------------------------------------------

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
