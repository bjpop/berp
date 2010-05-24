{-# LANGUAGE TemplateHaskell, MagicHash, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Hash
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Hashing functions.
--
-----------------------------------------------------------------------------

module Berp.Base.Hash (Hash (..), Hashed, hashedStr) where

import Berp.Base.Mangle (mangle)
import Language.Haskell.TH
import Data.HashTable as HT (hashString)
import GHC.Integer (hashInteger)
import GHC.Types (Int (..))

type Hashed a = (Int, a)

class Hash a where
   hash :: a -> Int

instance Hash String where
   hash = fromIntegral . HT.hashString

instance Hash Integer where
   hash i = I# (hashInteger i)

hashedStr :: String -> Q Exp
hashedStr str = [| (n, mangle str) |]
   where
   n :: Int
   n = hash str
