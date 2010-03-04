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

identString :: ToIdentString a => a -> String
identString = fromIdentString . toIdentString
