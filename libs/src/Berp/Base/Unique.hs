{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base
-- Copyright   : (c) Copyright 2002, The University Court of the University 
--               of Glasgow.  All rights reserved.  
-- License     : See the GHC license below. 
-- Maintainer  : (of this branch) florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A thread-safe supply of unique values.
-----------------------------------------------------------------------------

module Berp.Base.Unique (
   -- * Unique objects
   Unique (..),         -- instance (Eq, Ord)
   hashUnique,           -- :: Unique -> Int
   increment,
   zero
 ) where

import GHC.Base
import GHC.Num
import Berp.Base.Hash (Hash (..))

zero :: Unique
zero = Unique 0

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
newtype Unique = Unique { uniqueInteger :: Integer } deriving (Eq,Ord)

increment :: Unique -> Unique
increment (Unique i) = Unique (i+1)

-- not safe, but needed for printing purposes.
instance Show Unique where
   show (Unique i) = show i

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
hashUnique (Unique i) = I# (hashInteger i)

instance Hash Unique where
   hash = hashUnique
