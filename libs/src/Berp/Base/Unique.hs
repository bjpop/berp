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
-- Same as the version in GHC but with added show instance.
--
-----------------------------------------------------------------------------

{-
The Glasgow Haskell Compiler License

Copyright 2002, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
-}


module Berp.Base.Unique (
   -- * Unique objects
   Unique (..),         -- instance (Eq, Ord)
   newUnique,           -- :: IO Unique
   hashUnique           -- :: Unique -> Int
 ) where

import Prelude

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

import GHC.Base
import GHC.Num

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
newtype Unique = Unique { uniqueInteger :: Integer } deriving (Eq,Ord)

-- not safe, but needed for printing purposes. 
instance Show Unique where
   show (Unique i) = show i

{-# NOINLINE uniqSource #-}
uniqSource :: MVar Integer
uniqSource = unsafePerformIO (newMVar 0)

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = do
   val <- takeMVar uniqSource
   let next = val+1
   putMVar uniqSource next
   return (Unique next)

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
hashUnique (Unique i) = I# (hashInteger i)
