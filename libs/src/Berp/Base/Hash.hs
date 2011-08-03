{-# LANGUAGE MagicHash, TypeSynonymInstances #-}

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
import Data.HashTable as HT (hashString)

type Hashed a = (Int, a)

class Hash a where
   hash :: a -> Int

instance Hash String where
   hash = fromIntegral . HT.hashString

instance Hash Integer where
   hash = hashInteger

hashedStr :: String -> Hashed String
hashedStr str = (hash mangled, mangled)
   where
   mangled = mangle str

-- XXX May need to reconsider this. We'll want something which is fast.
hashInteger :: Integer -> Int
hashInteger i = fromInteger (i `mod` (toInteger (maxBound :: Int)))
